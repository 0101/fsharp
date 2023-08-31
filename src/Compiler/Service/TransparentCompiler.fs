namespace FSharp.Compiler.CodeAnalysis.TransparentCompiler

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Diagnostics
open System.IO

open Internal.Utilities.Collections
open Internal.Utilities.Library

open FSharp.Compiler
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.BuildGraph
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.CompilerOptions
open FSharp.Compiler.CheckBasics
open FSharp.Compiler.DependencyManager
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.DiagnosticsLogger
open FSharp.Compiler.IO
open FSharp.Compiler.ScriptClosure
open FSharp.Compiler.Symbols
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml
open System.Threading.Tasks
open FSharp.Compiler.ParseAndCheckInputs
open FSharp.Compiler.GraphChecking
open FSharp.Compiler.Syntax
open FSharp.Compiler.CompilerDiagnostics
open FSharp.Compiler.NameResolution
open Internal.Utilities.Library.Extras
open FSharp.Compiler.TypedTree
open FSharp.Compiler.CheckDeclarations
open FSharp.Compiler.EditorServices
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.CreateILModule
open FSharp.Compiler.TypedTreeOps


type internal FSharpFile =
    {
        Range: range
        Source: FSharpFileSnapshot
        IsLastCompiland: bool
        IsExe: bool
    }

/// Accumulated results of type checking. The minimum amount of state in order to continue type-checking following files.
[<NoEquality; NoComparison>]
type internal TcInfo =
    {
        tcState: TcState
        tcEnvAtEndOfFile: TcEnv

        /// Disambiguation table for module names
        moduleNamesDict: ModuleNamesDict

        topAttribs: TopAttribs option

        latestCcuSigForFile: ModuleOrNamespaceType option

        /// Accumulated diagnostics, last file first
        tcDiagnosticsRev: (PhasedDiagnostic * FSharpDiagnosticSeverity)[] list

        tcDependencyFiles: string list

        sigNameOpt: (string * QualifiedNameOfFile) option

        graphNode: NodeToTypeCheck option

        stateContainsNodes: Set<NodeToTypeCheck>

        sink: TcResultsSinkImpl list
    }

    member x.TcDiagnostics = Array.concat (List.rev x.tcDiagnosticsRev)

[<NoEquality; NoComparison>]
type internal TcIntermediate =
    {
        finisher: Finisher<NodeToTypeCheck, TcState, PartialResult>
        //tcEnvAtEndOfFile: TcEnv

        /// Disambiguation table for module names
        moduleNamesDict: ModuleNamesDict

        /// Accumulated diagnostics, last file first
        tcDiagnosticsRev: (PhasedDiagnostic * FSharpDiagnosticSeverity)[] list

        tcDependencyFiles: string list

        sink: TcResultsSinkImpl
    }

/// Things we need to start parsing and checking files for a given project snapshot
type internal BootstrapInfo =
    {
        AssemblyName: string
        OutFile: string
        TcConfig: TcConfig
        TcImports: TcImports
        TcGlobals: TcGlobals
        InitialTcInfo: TcInfo
        SourceFiles: FSharpFile list
        LoadClosure: LoadClosure option
    }

    member this.GetFile fileName =
        this.SourceFiles
        |> List.tryFind (fun f -> f.Source.FileName = fileName)
        |> Option.defaultWith (fun _ ->
            failwith (
                $"File {fileName} not found in project snapshot. Files in project: \n\n"
                + (this.SourceFiles |> Seq.map (fun f -> f.Source.FileName) |> String.concat " \n")
            ))

type internal TcIntermediateResult = TcInfo * TcResultsSinkImpl * CheckedImplFile option * string

[<RequireQualifiedAccess>]
type internal DependencyGraphType =
    /// A dependency graph for a single file - it will be missing files which this file does not depend on
    | File
    /// A dependency graph for a project - it will contain all files in the project
    | Project

[<Extension>]
type internal Extensions =
    [<Extension>]
    static member Key(fileSnapshots: FSharpFileSnapshot list, ?extraKeyFlag) =
        
        { new ICacheKey<_, _> with
            member _.GetLabel() =
                let lastFile = fileSnapshots |> List.tryLast |> Option.map (fun f -> f.FileName |> shortPath) |> Option.defaultValue "[no file]"
                $"%d{fileSnapshots.Length} files ending with {lastFile}"
            member _.GetKey() =
                Md5Hasher.empty
                |> Md5Hasher.addStrings (fileSnapshots |> Seq.map (fun f -> f.FileName))
                |> pair extraKeyFlag
            member _.GetVersion() =
                Md5Hasher.empty
                |> Md5Hasher.addStrings (fileSnapshots |> Seq.map (fun f -> f.Version)) }

[<AutoOpen>]
module private TypeCheckingGraphProcessing =
    open FSharp.Compiler.GraphChecking.GraphProcessing

    // TODO Do we need to suppress some error logging if we
    // TODO apply the same partial results multiple times?
    // TODO Maybe we can enable logging only for the final fold
    /// <summary>
    /// Combine type-checking results of dependencies needed to type-check a 'higher' node in the graph
    /// </summary>
    /// <param name="emptyState">Initial state</param>
    /// <param name="deps">Direct dependencies of a node</param>
    /// <param name="transitiveDeps">Transitive dependencies of a node</param>
    /// <param name="folder">A way to fold a single result into existing state</param>
    let private combineResults
        (emptyState: TcInfo)
        (deps: ProcessedNode<NodeToTypeCheck, TcInfo * Finisher<NodeToTypeCheck, TcInfo, PartialResult>> array)
        (transitiveDeps: ProcessedNode<NodeToTypeCheck, TcInfo * Finisher<NodeToTypeCheck, TcInfo, PartialResult>> array)
        (folder: TcInfo -> Finisher<NodeToTypeCheck, TcInfo, PartialResult> -> TcInfo)
        : TcInfo =
        match deps with
        | [||] -> emptyState
        | _ ->
            // Instead of starting with empty state,
            // reuse state produced by the dependency with the biggest number of transitive dependencies.
            // This is to reduce the number of folds required to achieve the final state.
            let biggestDependency =
                let sizeMetric (node: ProcessedNode<_, _>) = node.Info.TransitiveDeps.Length
                deps |> Array.maxBy sizeMetric

            let firstState = biggestDependency.Result |> fst

            // Find items not already included in the state.
            let itemsPresent =
                set
                    [|
                        yield! biggestDependency.Info.TransitiveDeps
                        yield biggestDependency.Info.Item
                    |]

            let resultsToAdd =
                transitiveDeps
                |> Array.filter (fun dep -> itemsPresent.Contains dep.Info.Item = false)
                |> Array.distinctBy (fun dep -> dep.Info.Item)
                |> Array.sortWith (fun a b ->
                    // We preserve the order in which items are folded to the state.
                    match a.Info.Item, b.Info.Item with
                    | NodeToTypeCheck.PhysicalFile aIdx, NodeToTypeCheck.PhysicalFile bIdx
                    | NodeToTypeCheck.ArtificialImplFile aIdx, NodeToTypeCheck.ArtificialImplFile bIdx -> aIdx.CompareTo bIdx
                    | NodeToTypeCheck.PhysicalFile _, NodeToTypeCheck.ArtificialImplFile _ -> -1
                    | NodeToTypeCheck.ArtificialImplFile _, NodeToTypeCheck.PhysicalFile _ -> 1)
                |> Array.map (fun dep -> dep.Result |> snd)

            // Fold results not already included and produce the final state
            let state = Array.fold folder firstState resultsToAdd
            state

    /// <summary>
    /// Process a graph of items.
    /// A version of 'GraphProcessing.processGraph' with a signature specific to type-checking.
    /// </summary>
    let processTypeCheckingGraph
        (graph: Graph<NodeToTypeCheck>)
        (work: NodeToTypeCheck -> TcInfo -> Async<Finisher<NodeToTypeCheck, TcInfo, PartialResult>>)
        (emptyState: TcInfo)
        : Async<(int * PartialResult) list * TcInfo> =
        async {

            let workWrapper
                (getProcessedNode: NodeToTypeCheck
                                       -> ProcessedNode<NodeToTypeCheck, TcInfo * Finisher<NodeToTypeCheck, TcInfo, PartialResult>>)
                (node: NodeInfo<NodeToTypeCheck>)
                : Async<TcInfo * Finisher<NodeToTypeCheck, TcInfo, PartialResult>> =
                async {
                    let folder (state: TcInfo) (Finisher (finisher = finisher)) : TcInfo = finisher state |> snd
                    let deps = node.Deps |> Array.except [| node.Item |] |> Array.map getProcessedNode

                    let transitiveDeps =
                        node.TransitiveDeps
                        |> Array.except [| node.Item |]
                        |> Array.map getProcessedNode

                    let inputState = combineResults emptyState deps transitiveDeps folder

                    let! singleRes = work node.Item inputState
                    let state = folder inputState singleRes
                    return state, singleRes
                }

            let! results = processGraphAsync graph workWrapper

            let finalFileResults, state: (int * PartialResult) list * TcInfo =
                (([], emptyState),
                 results
                 |> Array.choose (fun (item, res) ->
                     match item with
                     | NodeToTypeCheck.ArtificialImplFile _ -> None
                     | NodeToTypeCheck.PhysicalFile file -> Some(file, res)))
                ||> Array.fold (fun (fileResults, state) (item, (_, Finisher (finisher = finisher))) ->
                    let fileResult, state = finisher state
                    (item, fileResult) :: fileResults, state)

            return finalFileResults, state
        }


type internal CompilerCaches() =

    member val ParseFile = AsyncMemoize(keepStrongly=1000, keepWeakly=2000, name="ParseFile")

    member val ParseAndCheckFileInProject = AsyncMemoize(name="ParseAndCheckFileInProject")

    member val ParseAndCheckAllFilesInProject = AsyncMemoize(name="ParseAndCheckFullProject")

    member val ParseAndCheckProject = AsyncMemoize(name="ParseAndCheckProject")

    member val FrameworkImports = AsyncMemoize(name="FrameworkImports")

    member val BootstrapInfoStatic = AsyncMemoize(name="BootstrapInfoStatic")

    member val BootstrapInfo = AsyncMemoize(name="BootstrapInfo")

    member val TcFile = AsyncMemoize(name="TcPrior")

    member val TcIntermediate = AsyncMemoize(keepStrongly=1000, keepWeakly=2000, name="TcIntermediate")

    member val DependencyGraph = AsyncMemoize(name="DependencyGraph")

    member val ProjectExtras = AsyncMemoize(name="ProjectExtras")

    member val AssemblyData = AsyncMemoize(name="AssemblyData")

    member val SemanticClassification = AsyncMemoize(name="SemanticClassification")

    member val ItemKeyStore = AsyncMemoize(name="ItemKeyStore") 


type internal TransparentCompiler
    (
        legacyReferenceResolver,
        projectCacheSize,
        keepAssemblyContents,
        keepAllBackgroundResolutions,
        tryGetMetadataSnapshot,
        suggestNamesForErrors,
        keepAllBackgroundSymbolUses,
        enableBackgroundItemKeyStoreAndSemanticClassification,
        enablePartialTypeChecking,
        parallelReferenceResolution,
        captureIdentifiersWhenParsing,
        getSource: (string -> Async<ISourceText option>) option,
        useChangeNotifications,
        useSyntaxTreeCache
    ) as self =

    // Is having just one of these ok?
    let lexResourceManager = Lexhelp.LexResourceManager()

    let caches = CompilerCaches()

    // We currently share one global dependency provider for all scripts for the FSharpChecker.
    // For projects, one is used per project.
    //
    // Sharing one for all scripts is necessary for good performance from GetProjectOptionsFromScript,
    // which requires a dependency provider to process through the project options prior to working out
    // if the cached incremental builder can be used for the project.
    let dependencyProviderForScripts = new DependencyProvider()

    // use this to process not-yet-implemented tasks
    let backgroundCompiler =
        BackgroundCompiler(
            legacyReferenceResolver,
            projectCacheSize,
            keepAssemblyContents,
            keepAllBackgroundResolutions,
            tryGetMetadataSnapshot,
            suggestNamesForErrors,
            keepAllBackgroundSymbolUses,
            enableBackgroundItemKeyStoreAndSemanticClassification,
            enablePartialTypeChecking,
            parallelReferenceResolution,
            captureIdentifiersWhenParsing,
            getSource,
            useChangeNotifications,
            useSyntaxTreeCache
        )
        :> IBackgroundCompiler

    let ComputeFrameworkImports (tcConfig: TcConfig) frameworkDLLs nonFrameworkResolutions =
        let frameworkDLLsKey =
            frameworkDLLs
            |> List.map (fun ar -> ar.resolvedPath) // The cache key. Just the minimal data.
            |> List.sort // Sort to promote cache hits.

        // The data elements in this key are very important. There should be nothing else in the TcConfig that logically affects
        // the import of a set of framework DLLs into F# CCUs. That is, the F# CCUs that result from a set of DLLs (including
        // FSharp.Core.dll and mscorlib.dll) must be logically invariant of all the other compiler configuration parameters.
        let key =
            FrameworkImportsCacheKey(
                frameworkDLLsKey,
                tcConfig.primaryAssembly.Name,
                tcConfig.GetTargetFrameworkDirectories(),
                tcConfig.fsharpBinariesDir,
                tcConfig.langVersion.SpecifiedVersion
            )

        caches.FrameworkImports.Get(
            key,
            async {
                use _ = Activity.start "ComputeFrameworkImports" []
                let tcConfigP = TcConfigProvider.Constant tcConfig
                return! TcImports.BuildFrameworkTcImports(tcConfigP, frameworkDLLs, nonFrameworkResolutions) |> Async.AwaitNodeCode
            }
        )

    // Link all the assemblies together and produce the input typecheck accumulator
    let CombineImportedAssembliesTask
        (
            assemblyName,
            tcConfig: TcConfig,
            tcConfigP,
            tcGlobals,
            frameworkTcImports,
            nonFrameworkResolutions,
            unresolvedReferences,
            dependencyProvider,
            loadClosureOpt: LoadClosure option,
            basicDependencies,
            importsInvalidatedByTypeProvider: Event<unit>
        ) =

        async {
            let diagnosticsLogger =
                CompilationDiagnosticLogger("CombineImportedAssembliesTask", tcConfig.diagnosticsOptions)

            use _ = new CompilationGlobalsScope(diagnosticsLogger, BuildPhase.Parameter)

            let! tcImports =
                async {
                    try
                        let! tcImports =
                            TcImports.BuildNonFrameworkTcImports(
                                tcConfigP,
                                frameworkTcImports,
                                nonFrameworkResolutions,
                                unresolvedReferences,
                                dependencyProvider
                            ) |> Async.AwaitNodeCode
#if !NO_TYPEPROVIDERS
                        // TODO: review and handle the event
                        tcImports.GetCcusExcludingBase()
                        |> Seq.iter (fun ccu ->
                            // When a CCU reports an invalidation, merge them together and just report a
                            // general "imports invalidated". This triggers a rebuild.
                            //
                            // We are explicit about what the handler closure captures to help reason about the
                            // lifetime of captured objects, especially in case the type provider instance gets leaked
                            // or keeps itself alive mistakenly, e.g. via some global state in the type provider instance.
                            //
                            // The handler only captures
                            //    1. a weak reference to the importsInvalidated event.
                            //
                            // The IncrementalBuilder holds the strong reference the importsInvalidated event.
                            //
                            // In the invalidation handler we use a weak reference to allow the IncrementalBuilder to
                            // be collected if, for some reason, a TP instance is not disposed or not GC'd.
                            let capturedImportsInvalidated = WeakReference<_>(importsInvalidatedByTypeProvider)

                            ccu.Deref.InvalidateEvent.Add(fun _ ->
                                match capturedImportsInvalidated.TryGetTarget() with
                                | true, tg -> tg.Trigger()
                                | _ -> ()))
#endif
#if NO_TYPEPROVIDERS
                        ignore importsInvalidatedByTypeProvider
#endif
                        return tcImports
                    with
                    | :? OperationCanceledException ->
                        // if it's been canceled then it shouldn't be needed anymore
                        return frameworkTcImports
                    | exn ->
                        Debug.Assert(false, sprintf "Could not BuildAllReferencedDllTcImports %A" exn)
                        diagnosticsLogger.Warning exn
                        return frameworkTcImports
                }

            let tcInitial, openDecls0 =
                GetInitialTcEnv(assemblyName, rangeStartup, tcConfig, tcImports, tcGlobals)

            let tcState =
                GetInitialTcState(rangeStartup, assemblyName, tcConfig, tcGlobals, tcImports, tcInitial, openDecls0)

            let loadClosureErrors =
                [
                    match loadClosureOpt with
                    | None -> ()
                    | Some loadClosure ->
                        for inp in loadClosure.Inputs do
                            yield! inp.MetaCommandDiagnostics
                ]

            let initialErrors =
                Array.append (Array.ofList loadClosureErrors) (diagnosticsLogger.GetDiagnostics())

            let tcInfo =
                {
                    tcState = tcState
                    tcEnvAtEndOfFile = tcInitial
                    topAttribs = None
                    latestCcuSigForFile = None
                    tcDiagnosticsRev = [ initialErrors ]
                    moduleNamesDict = Map.empty
                    tcDependencyFiles = basicDependencies
                    sigNameOpt = None
                    graphNode = None
                    stateContainsNodes = Set.empty
                    sink = []
                }

            return tcImports, tcInfo
        }

    let getProjectReferences (project: FSharpProjectSnapshot) userOpName =
        [
            for r in project.ReferencedProjects do

                match r with
                | FSharpReferencedProjectSnapshot.FSharpReference (nm, projectSnapshot) ->
                    // Don't use cross-project references for FSharp.Core, since various bits of code
                    // require a concrete FSharp.Core to exist on-disk. The only solutions that have
                    // these cross-project references to FSharp.Core are VisualFSharp.sln and FSharp.sln. The ramification
                    // of this is that you need to build FSharp.Core to get intellisense in those projects.

                    if
                        (try
                            Path.GetFileNameWithoutExtension(nm)
                         with _ ->
                             "")
                        <> GetFSharpCoreLibraryName()
                    then
                        { new IProjectReference with
                            member x.EvaluateRawContents() =
                                node {
                                    Trace.TraceInformation("FCS: {0}.{1} ({2})", userOpName, "GetAssemblyData", nm)

                                    return! self.GetAssemblyData(projectSnapshot, userOpName + ".CheckReferencedProject(" + nm + ")")
                                }

                            member x.TryGetLogicalTimeStamp(cache) =
                                // TODO:
                                None

                            member x.FileName = nm
                        }
        ]

    /// Bootstrap info that does not depend on contents of the files
    let ComputeBootstrapInfoStatic (projectSnapshot: FSharpProjectSnapshot) =

        let key = projectSnapshot.WithoutFileVersions.Key

        caches.BootstrapInfoStatic.Get(
            key,
            async {
                use _ =
                    Activity.start
                        "ComputeBootstrapInfoStatic"
                        [| Activity.Tags.project, projectSnapshot.ProjectFileName |> Path.GetFileName |]

                let useSimpleResolutionSwitch = "--simpleresolution"
                let commandLineArgs = projectSnapshot.CommandLineOptions
                let defaultFSharpBinariesDir = FSharpCheckerResultsSettings.defaultFSharpBinariesDir
                let useScriptResolutionRules = projectSnapshot.UseScriptResolutionRules

                let projectReferences = getProjectReferences projectSnapshot "ComputeBootstrapInfo"

                // TODO: script support
                let loadClosureOpt: LoadClosure option = None

                let tcConfigB, sourceFiles =

                    let getSwitchValue (switchString: string) =
                        match commandLineArgs |> List.tryFindIndex (fun s -> s.StartsWithOrdinal switchString) with
                        | Some idx -> Some(commandLineArgs[ idx ].Substring(switchString.Length))
                        | _ -> None

                    let sdkDirOverride =
                        match loadClosureOpt with
                        | None -> None
                        | Some loadClosure -> loadClosure.SdkDirOverride

                    // see also fsc.fs: runFromCommandLineToImportingAssemblies(), as there are many similarities to where the PS creates a tcConfigB
                    let tcConfigB =
                        TcConfigBuilder.CreateNew(
                            legacyReferenceResolver,
                            defaultFSharpBinariesDir,
                            implicitIncludeDir = projectSnapshot.ProjectDirectory,
                            reduceMemoryUsage = ReduceMemoryFlag.Yes,
                            isInteractive = useScriptResolutionRules,
                            isInvalidationSupported = true,
                            defaultCopyFSharpCore = CopyFSharpCoreFlag.No,
                            tryGetMetadataSnapshot = tryGetMetadataSnapshot,
                            sdkDirOverride = sdkDirOverride,
                            rangeForErrors = range0
                        )

                    tcConfigB.primaryAssembly <-
                        match loadClosureOpt with
                        | None -> PrimaryAssembly.Mscorlib
                        | Some loadClosure ->
                            if loadClosure.UseDesktopFramework then
                                PrimaryAssembly.Mscorlib
                            else
                                PrimaryAssembly.System_Runtime

                    tcConfigB.resolutionEnvironment <- (LegacyResolutionEnvironment.EditingOrCompilation true)

                    tcConfigB.conditionalDefines <-
                        let define =
                            if useScriptResolutionRules then
                                "INTERACTIVE"
                            else
                                "COMPILED"

                        define :: tcConfigB.conditionalDefines

                    tcConfigB.projectReferences <- projectReferences

                    tcConfigB.useSimpleResolution <- (getSwitchValue useSimpleResolutionSwitch) |> Option.isSome

                    // Apply command-line arguments and collect more source files if they are in the arguments
                    let sourceFilesNew =
                        ApplyCommandLineArgs(tcConfigB, projectSnapshot.SourceFileNames, commandLineArgs)

                    // Never open PDB files for the language service, even if --standalone is specified
                    tcConfigB.openDebugInformationForLaterStaticLinking <- false

                    tcConfigB.xmlDocInfoLoader <-
                        { new IXmlDocumentationInfoLoader with
                            /// Try to load xml documentation associated with an assembly by the same file path with the extension ".xml".
                            member _.TryLoad(assemblyFileName) =
                                let xmlFileName = Path.ChangeExtension(assemblyFileName, ".xml")

                                // REVIEW: File IO - Will eventually need to change this to use a file system interface of some sort.
                                XmlDocumentationInfo.TryCreateFromFile(xmlFileName)
                        }
                        |> Some

                    tcConfigB.parallelReferenceResolution <- parallelReferenceResolution
                    tcConfigB.captureIdentifiersWhenParsing <- captureIdentifiersWhenParsing

                    tcConfigB, sourceFilesNew

                // If this is a builder for a script, re-apply the settings inferred from the
                // script and its load closure to the configuration.
                //
                // NOTE: it would probably be cleaner and more accurate to re-run the load closure at this point.
                let setupConfigFromLoadClosure () =
                    match loadClosureOpt with
                    | Some loadClosure ->
                        let dllReferences =
                            [
                                for reference in tcConfigB.referencedDLLs do
                                    // If there's (one or more) resolutions of closure references then yield them all
                                    match
                                        loadClosure.References
                                        |> List.tryFind (fun (resolved, _) -> resolved = reference.Text)
                                    with
                                    | Some (resolved, closureReferences) ->
                                        for closureReference in closureReferences do
                                            yield AssemblyReference(closureReference.originalReference.Range, resolved, None)
                                    | None -> yield reference
                            ]

                        tcConfigB.referencedDLLs <- []

                        tcConfigB.primaryAssembly <-
                            (if loadClosure.UseDesktopFramework then
                                 PrimaryAssembly.Mscorlib
                             else
                                 PrimaryAssembly.System_Runtime)
                        // Add one by one to remove duplicates
                        dllReferences
                        |> List.iter (fun dllReference -> tcConfigB.AddReferencedAssemblyByPath(dllReference.Range, dllReference.Text))

                        tcConfigB.knownUnresolvedReferences <- loadClosure.UnresolvedReferences
                    | None -> ()

                setupConfigFromLoadClosure ()

                let tcConfig = TcConfig.Create(tcConfigB, validate = true)
                let outfile, _, assemblyName = tcConfigB.DecideNames sourceFiles

                // Resolve assemblies and create the framework TcImports. This caches a level of "system" references. No type providers are
                // included in these references.

                let frameworkDLLs, nonFrameworkResolutions, unresolvedReferences =
                    TcAssemblyResolutions.SplitNonFoundationalResolutions(tcConfig)

                // Prepare the frameworkTcImportsCache
                let! tcGlobals, frameworkTcImports = ComputeFrameworkImports tcConfig frameworkDLLs nonFrameworkResolutions

                // Note we are not calling diagnosticsLogger.GetDiagnostics() anywhere for this task.
                // This is ok because not much can actually go wrong here.
                let diagnosticsLogger =
                    CompilationDiagnosticLogger("nonFrameworkAssemblyInputs", tcConfig.diagnosticsOptions)

                use _ = new CompilationGlobalsScope(diagnosticsLogger, BuildPhase.Parameter)

                let tcConfigP = TcConfigProvider.Constant tcConfig

                let importsInvalidatedByTypeProvider = Event<unit>()

                // Check for the existence of loaded sources and prepend them to the sources list if present.
                let sourceFiles =
                    tcConfig.GetAvailableLoadedSources()
                    @ (sourceFiles |> List.map (fun s -> rangeStartup, s))

                // Mark up the source files with an indicator flag indicating if they are the last source file in the project
                let sourceFiles =
                    let flags, isExe = tcConfig.ComputeCanContainEntryPoint(sourceFiles |> List.map snd)
                    ((sourceFiles, flags) ||> List.map2 (fun (m, nm) flag -> (m, nm, (flag, isExe))))

                let basicDependencies =
                    [
                        for UnresolvedAssemblyReference (referenceText, _) in unresolvedReferences do
                            // Exclude things that are definitely not a file name
                            if not (FileSystem.IsInvalidPathShim referenceText) then
                                let file =
                                    if FileSystem.IsPathRootedShim referenceText then
                                        referenceText
                                    else
                                        Path.Combine(projectSnapshot.ProjectDirectory, referenceText)

                                yield file

                        for r in nonFrameworkResolutions do
                            yield r.resolvedPath
                    ]

                // For scripts, the dependency provider is already available.
                // For projects create a fresh one for the project.
                let dependencyProvider =
                    if projectSnapshot.UseScriptResolutionRules then
                        dependencyProviderForScripts
                    else
                        new DependencyProvider()

                let! tcImports, initialTcInfo =
                    CombineImportedAssembliesTask(
                        assemblyName,
                        tcConfig,
                        tcConfigP,
                        tcGlobals,
                        frameworkTcImports,
                        nonFrameworkResolutions,
                        unresolvedReferences,
                        dependencyProvider,
                        loadClosureOpt,
                        basicDependencies,
                        importsInvalidatedByTypeProvider
                    )

                return
                    assemblyName,
                    outfile,
                    sourceFiles,
                    tcConfig,
                    tcImports,
                    tcGlobals,
                    initialTcInfo,
                    loadClosureOpt,
                    importsInvalidatedByTypeProvider
            }
        )

    let computeBootstrapInfoInner (projectSnapshot: FSharpProjectSnapshot) =
        async {
            let! assemblyName,
                 outFile,
                 sourceFiles,
                 tcConfig,
                 tcImports,
                 tcGlobals,
                 initialTcInfo,
                 loadClosureOpt,
                 _importsInvalidatedByTypeProvider = ComputeBootstrapInfoStatic projectSnapshot

            let fileSnapshots = Map [ for f in projectSnapshot.SourceFiles -> f.FileName, f ]

            let sourceFiles =
                sourceFiles
                |> List.map (fun (m, fileName, (isLastCompiland, isExe)) ->
                    let source =
                        fileSnapshots.TryFind fileName
                        |> Option.defaultWith (fun () ->
                            // TODO: does this commonly happen?
                            {
                                FileName = fileName
                                Version = (FileSystem.GetLastWriteTimeShim fileName).Ticks.ToString()
                                GetSource = (fun () -> fileName |> File.ReadAllText |> SourceText.ofString |> Task.FromResult)
                            })

                    {
                        Range = m
                        Source = source
                        IsLastCompiland = isLastCompiland
                        IsExe = isExe
                    })

            return
                Some
                    {
                        AssemblyName = assemblyName
                        OutFile = outFile
                        TcConfig = tcConfig
                        TcImports = tcImports
                        TcGlobals = tcGlobals
                        InitialTcInfo = initialTcInfo
                        SourceFiles = sourceFiles
                        LoadClosure = loadClosureOpt
                    //ImportsInvalidatedByTypeProvider = importsInvalidatedByTypeProvider
                    }
        }

    let ComputeBootstrapInfo (projectSnapshot: FSharpProjectSnapshot) =
        caches.BootstrapInfo.Get(
            projectSnapshot.Key,
            async {
                use _ =
                    Activity.start "ComputeBootstrapInfo" [| Activity.Tags.project, projectSnapshot.ProjectFileName |> Path.GetFileName |]

                // Trap and report diagnostics from creation.
                let delayedLogger = CapturingDiagnosticsLogger("IncrementalBuilderCreation")
                use _ = new CompilationGlobalsScope(delayedLogger, BuildPhase.Parameter)

                let! bootstrapInfoOpt =
                    async {
                        try
                            return! computeBootstrapInfoInner projectSnapshot
                        with exn ->
                            errorRecoveryNoRange exn
                            return None
                    }

                let diagnostics =
                    match bootstrapInfoOpt with
                    | Some bootstrapInfo ->
                        let diagnosticsOptions = bootstrapInfo.TcConfig.diagnosticsOptions

                        let diagnosticsLogger =
                            CompilationDiagnosticLogger("IncrementalBuilderCreation", diagnosticsOptions)

                        delayedLogger.CommitDelayedDiagnostics diagnosticsLogger
                        diagnosticsLogger.GetDiagnostics()
                    | _ -> Array.ofList delayedLogger.Diagnostics
                    |> Array.map (fun (diagnostic, severity) ->
                        let flatErrors =
                            bootstrapInfoOpt
                            |> Option.map (fun bootstrapInfo -> bootstrapInfo.TcConfig.flatErrors)
                            |> Option.defaultValue false // TODO: do we need to figure this out?

                        FSharpDiagnostic.CreateFromException(diagnostic, severity, range.Zero, suggestNamesForErrors, flatErrors))

                return bootstrapInfoOpt, diagnostics
            }
        )

    let ComputeParseFile bootstrapInfo (file: FSharpFile) =
        
        let key =
            { new ICacheKey<_, _> with
                member _.GetLabel() = shortPath file.Source.FileName 
                member _.GetKey() = file.Source.FileName
                member _.GetVersion() = file.Source.Version, file.IsLastCompiland, file.IsExe }

        caches.ParseFile.Get(
            key,
            async {
                use _ =
                    Activity.start
                        "ComputeParseFile"
                        [|
                            Activity.Tags.fileName, file.Source.FileName |> Path.GetFileName
                            Activity.Tags.version, file.Source.Version
                        |]

                let tcConfig = bootstrapInfo.TcConfig

                let diagnosticsLogger =
                    CompilationDiagnosticLogger("Parse", tcConfig.diagnosticsOptions)
                // Return the disposable object that cleans up
                use _holder = new CompilationGlobalsScope(diagnosticsLogger, BuildPhase.Parse)

                let flags = file.IsLastCompiland, file.IsExe
                let fileName = file.Source.FileName
                let! sourceText = file.Source.GetSource() |> Async.AwaitTask

                let input =
                    ParseOneInputSourceText(tcConfig, lexResourceManager, fileName, flags, diagnosticsLogger, sourceText)

                return input, diagnosticsLogger.GetDiagnostics(), sourceText
            }
        )

    // In case we don't want to use any parallel processing
    let mkLinearGraph count : Graph<FileIndex> =
        seq {        
            0, [| |]
            yield!
                [0..count-1]
                |> Seq.rev
                |> Seq.pairwise
                |> Seq.map (fun (a, b) -> a, [| b |])
        } 
        |> Graph.make

    let computeDependencyGraph (tcConfig: TcConfig) parsedInputs (processGraph: Graph<FileIndex> -> Graph<FileIndex>) =
        async {
            let sourceFiles: FileInProject array =
                parsedInputs
                |> Seq.toArray
                |> Array.mapi (fun idx (input: ParsedInput) ->
                    {
                        Idx = idx
                        FileName = input.FileName
                        ParsedInput = input
                    })

            use _ =
                Activity.start "ComputeDependencyGraph" [| Activity.Tags.fileName, (sourceFiles |> Array.last).FileName |]

            let filePairs = FilePairMap(sourceFiles)

            // TODO: we will probably want to cache and re-use larger graphs if available

            let graph =
                if tcConfig.compilingFSharpCore then 
                    mkLinearGraph sourceFiles.Length
                else
                    DependencyResolution.mkGraph filePairs sourceFiles
                    |> fst
                    |> processGraph

            let nodeGraph = TransformDependencyGraph(graph, filePairs)

            let fileNames =
                parsedInputs
                |> Seq.mapi (fun idx input -> idx, Path.GetFileName input.FileName)
                |> Map.ofSeq

            let debugGraph =
                nodeGraph
                |> Graph.map (function
                    | NodeToTypeCheck.PhysicalFile i -> i, $"[{i}] {fileNames[i]}"
                    | NodeToTypeCheck.ArtificialImplFile i -> -(i + 1), $"AIF [{i}] : {fileNames[i]}")
                |> Graph.serialiseToMermaid

            Trace.TraceInformation("\n" + debugGraph)

            if Activity.Current <> null then
                Activity.Current.AddTag("graph", debugGraph) |> ignore

            return nodeGraph, graph
        }

    let removeImplFilesThatHaveSignatures (projectSnapshot: FSharpProjectSnapshot) (graph: Graph<FileIndex>) = 

        let removeIndexes =
            projectSnapshot.SourceFileNames
            |> Seq.mapi pair
            |> Seq.groupBy (snd >> (fun fileName -> if fileName.EndsWith(".fsi") then fileName.Substring(0, fileName.Length - 1) else fileName))
            |> Seq.map (snd >> (Seq.toList))
            |> Seq.choose (function [idx1, _; idx2, _] -> max idx1 idx2 |> Some | _ -> None)
            |> Set

        graph
        |> Seq.filter (fun x -> not (removeIndexes.Contains x.Key))
        |> Seq.map (fun x -> x.Key, x.Value |> Array.filter (fun node -> not (removeIndexes.Contains node)))
        |> Graph.make

    let removeImplFilesThatHaveSignaturesExceptLastOne (projectSnapshot: FSharpProjectSnapshot) (graph: Graph<FileIndex>) = 

        let removeIndexes =
            projectSnapshot.SourceFileNames
            |> Seq.mapi pair
            |> Seq.groupBy (snd >> (fun fileName -> if fileName.EndsWith(".fsi") then fileName.Substring(0, fileName.Length - 1) else fileName))
            |> Seq.map (snd >> (Seq.toList))
            |> Seq.choose (function [idx1, _; idx2, _] -> max idx1 idx2 |> Some | _ -> None)
            |> Set
            // Don't remove the last file
            |> Set.remove (projectSnapshot.SourceFiles.Length - 1)

        graph
        |> Seq.filter (fun x -> not (removeIndexes.Contains x.Key))
        |> Seq.map (fun x -> x.Key, x.Value |> Array.filter (fun node -> not (removeIndexes.Contains node)))
        |> Graph.make

    let ComputeDependencyGraphForFile (tcConfig: TcConfig) (priorSnapshot: FSharpProjectSnapshot) parsedInputs =
        let key = priorSnapshot.SourceFiles.Key(DependencyGraphType.File) 
        //let lastFileIndex = (parsedInputs |> Array.length) - 1
        //caches.DependencyGraph.Get(key, computeDependencyGraph parsedInputs (Graph.subGraphFor lastFileIndex))
        caches.DependencyGraph.Get(key, computeDependencyGraph tcConfig parsedInputs (removeImplFilesThatHaveSignaturesExceptLastOne priorSnapshot))

    let ComputeDependencyGraphForProject (tcConfig: TcConfig) (projectSnapshot: FSharpProjectSnapshot) parsedInputs =
        let key = projectSnapshot.SourceFiles.Key(DependencyGraphType.Project)
        //caches.DependencyGraph.Get(key, computeDependencyGraph parsedInputs (removeImplFilesThatHaveSignatures projectSnapshot))
        caches.DependencyGraph.Get(key, computeDependencyGraph tcConfig parsedInputs id)

    let ComputeTcIntermediate
        (projectSnapshot: FSharpProjectSnapshot)
        (dependencyGraph: Graph<FileIndex>)
        (node: NodeToTypeCheck)
        (parsedInput: ParsedInput, parseErrors)
        bootstrapInfo
        (prevTcInfo: TcInfo)
        =

        ignore dependencyGraph

        let key = projectSnapshot.FileKey parsedInput.FileName

        caches.TcIntermediate.Get(
            key,
            async {
                let input = parsedInput
                let fileName = input.FileName

                use _ =
                    Activity.start "ComputeTcIntermediate" [| 
                        Activity.Tags.fileName, fileName |> Path.GetFileName 
                        "key", key.GetKey() |> sprintf "%A"
                        "version", key.GetVersion()
                    |]

                let tcConfig = bootstrapInfo.TcConfig
                let tcGlobals = bootstrapInfo.TcGlobals
                let tcImports = bootstrapInfo.TcImports

                let capturingDiagnosticsLogger = CapturingDiagnosticsLogger("TypeCheck")

                let diagnosticsLogger =
                    GetDiagnosticsLoggerFilteringByScopedPragmas(
                        false,
                        input.ScopedPragmas,
                        tcConfig.diagnosticsOptions,
                        capturingDiagnosticsLogger
                    )

                use _ = new CompilationGlobalsScope(diagnosticsLogger, BuildPhase.TypeCheck)

                //beforeFileChecked.Trigger fileName

                ApplyMetaCommandsFromInputToTcConfig(tcConfig, input, Path.GetDirectoryName fileName, tcImports.DependencyProvider)
                |> ignore

                let sink = TcResultsSinkImpl(tcGlobals)

                let hadParseErrors = not (Array.isEmpty parseErrors)

                let input, moduleNamesDict =
                    DeduplicateParsedInputModuleName prevTcInfo.moduleNamesDict input

                let! finisher =
                    CheckOneInputWithCallback
                        node
                        ((fun () -> hadParseErrors || diagnosticsLogger.ErrorCount > 0),
                         tcConfig,
                         tcImports,
                         tcGlobals,
                         None,
                         TcResultsSink.WithSink sink,
                         prevTcInfo.tcState,
                         input,
                         true)
                         |> Cancellable.toAsync

                //fileChecked.Trigger fileName

                let newErrors =
                    Array.append parseErrors (capturingDiagnosticsLogger.Diagnostics |> List.toArray)

                return
                    {
                        finisher = finisher
                        moduleNamesDict = moduleNamesDict
                        tcDiagnosticsRev = [ newErrors ]
                        tcDependencyFiles = [ fileName ]
                        sink = sink
                    }
            }
        )

    let processGraphNode
        projectSnapshot
        bootstrapInfo
        (parsedInputs: _ array)
        dependencyFiles
        collectSinks
        (fileNode: NodeToTypeCheck)
        tcInfo
        =
        async {

            match fileNode with
            | NodeToTypeCheck.PhysicalFile index ->
                let input, parseErrors, _ = parsedInputs[index]
                let! tcIntermediate = ComputeTcIntermediate projectSnapshot dependencyFiles fileNode (input, parseErrors) bootstrapInfo tcInfo
                let (Finisher (node = node; finisher = finisher)) = tcIntermediate.finisher

                return
                    Finisher(
                        node,
                        (fun tcInfo ->

                            if tcInfo.stateContainsNodes |> Set.contains fileNode then
                                failwith $"Oops!"

                            if tcInfo.stateContainsNodes |> Set.contains (NodeToTypeCheck.ArtificialImplFile (index - 1)) then
                                failwith $"Oops???"

                            let partialResult, tcState = finisher tcInfo.tcState

                            let tcEnv, topAttribs, _checkImplFileOpt, ccuSigForFile = partialResult

                            let tcEnvAtEndOfFile =
                                if keepAllBackgroundResolutions then
                                    tcEnv
                                else
                                    tcState.TcEnvFromImpls

                            partialResult,
                            { tcInfo with
                                tcState = tcState
                                tcEnvAtEndOfFile = tcEnvAtEndOfFile
                                moduleNamesDict = tcIntermediate.moduleNamesDict
                                topAttribs = Some topAttribs
                                tcDiagnosticsRev = tcIntermediate.tcDiagnosticsRev @ tcInfo.tcDiagnosticsRev
                                tcDependencyFiles = tcIntermediate.tcDependencyFiles @ tcInfo.tcDependencyFiles
                                latestCcuSigForFile = Some ccuSigForFile
                                graphNode = Some node
                                stateContainsNodes = tcInfo.stateContainsNodes |> Set.add node
                                sink =
                                    if collectSinks then
                                        tcIntermediate.sink :: tcInfo.sink
                                    else
                                        [ tcIntermediate.sink ]
                            })
                    )

            | NodeToTypeCheck.ArtificialImplFile index ->
                return
                    Finisher(
                        fileNode,
                        (fun tcInfo ->

                            if tcInfo.stateContainsNodes |> Set.contains fileNode then
                                failwith $"Oops!"

                            if tcInfo.stateContainsNodes |> Set.contains (NodeToTypeCheck.PhysicalFile (index + 1)) then
                                failwith $"Oops!!!"

                            let parsedInput, _parseErrors, _ = parsedInputs[index]
                            let prefixPathOpt = None
                            // Retrieve the type-checked signature information and add it to the TcEnvFromImpls.
                            let partialResult, tcState =
                                AddSignatureResultToTcImplEnv
                                    (bootstrapInfo.TcImports,
                                     bootstrapInfo.TcGlobals,
                                     prefixPathOpt,
                                     TcResultsSink.NoSink,
                                     tcInfo.tcState,
                                     parsedInput)
                                    tcInfo.tcState

                            let tcEnv, topAttribs, _checkImplFileOpt, ccuSigForFile = partialResult

                            let tcEnvAtEndOfFile =
                                if keepAllBackgroundResolutions then
                                    tcEnv
                                else
                                    tcState.TcEnvFromImpls

                            partialResult,
                            { tcInfo with
                                tcState = tcState
                                tcEnvAtEndOfFile = tcEnvAtEndOfFile
                                topAttribs = Some topAttribs
                                latestCcuSigForFile = Some ccuSigForFile
                                graphNode = Some fileNode
                                stateContainsNodes = tcInfo.stateContainsNodes |> Set.add fileNode
                            })
                    )

        }

    // Type check file and all its dependencies
    let ComputeTcFile (file: FSharpFile) (bootstrapInfo: BootstrapInfo) (projectSnapshot: FSharpProjectSnapshot) =

        let priorSnapshot = projectSnapshot.UpTo file.Source.FileName

        caches.TcFile.Get(
            priorSnapshot.FileKey file.Source.FileName,
            async {
                use _ =
                    Activity.start "ComputeTcFile" [| Activity.Tags.fileName, file.Source.FileName |> Path.GetFileName |]

                // parse required files
                let files =
                    seq {
                        yield! bootstrapInfo.SourceFiles |> Seq.takeWhile ((<>) file)
                        file
                    }

                let! parsedInputs = files |> Seq.map (ComputeParseFile bootstrapInfo) |> Async.Parallel

                let! graph, dependencyFiles =
                    ComputeDependencyGraphForFile bootstrapInfo.TcConfig priorSnapshot (parsedInputs |> Array.map p13)
                    //ComputeDependencyGraphForProject priorSnapshot (parsedInputs |> Array.map p13)

                let! results, tcInfo =
                    processTypeCheckingGraph
                        graph
                        (processGraphNode projectSnapshot bootstrapInfo parsedInputs dependencyFiles false)
                        bootstrapInfo.InitialTcInfo

                let lastResult = results |> List.head |> snd

                return lastResult, tcInfo
            }
        )

    let getParseResult (bootstrapInfo: BootstrapInfo) creationDiags fileName =
        async {
            let file = bootstrapInfo.GetFile fileName

            let! parseTree, parseDiagnostics, sourceText = ComputeParseFile bootstrapInfo file

            let parseDiagnostics =
                DiagnosticHelpers.CreateDiagnostics(
                    bootstrapInfo.TcConfig.diagnosticsOptions,
                    false,
                    fileName,
                    parseDiagnostics,
                    suggestNamesForErrors,
                    bootstrapInfo.TcConfig.flatErrors
                )

            let diagnostics = [| yield! creationDiags; yield! parseDiagnostics |]

            return
                FSharpParseFileResults(
                    diagnostics = diagnostics,
                    input = parseTree,
                    parseHadErrors = (parseDiagnostics.Length > 0),
                    // TODO: check if we really need this in parse results
                    dependencyFiles = [||]
                ),
                sourceText
        }

    let emptyParseResult fileName diagnostics =
        let parseTree = EmptyParsedInput(fileName, (false, false))
        FSharpParseFileResults(diagnostics, parseTree, true, [||])

    let ComputeParseAndCheckFileInProject (fileName: string) (projectSnapshot: FSharpProjectSnapshot) =
        caches.ParseAndCheckFileInProject.Get(
            projectSnapshot.FileKey fileName,
            async {

                use _ =
                    Activity.start "ComputeParseAndCheckFileInProject" [| Activity.Tags.fileName, fileName |> Path.GetFileName |]

                match! ComputeBootstrapInfo projectSnapshot with
                | None, creationDiags -> return emptyParseResult fileName creationDiags, FSharpCheckFileAnswer.Aborted

                | Some bootstrapInfo, creationDiags ->

                    let file = bootstrapInfo.GetFile fileName

                    let! parseResults, _sourceText = getParseResult bootstrapInfo creationDiags fileName

                    let! result, priorTcInfo = ComputeTcFile file bootstrapInfo projectSnapshot

                    let (tcEnv, _topAttribs, checkedImplFileOpt, ccuSigForFile) = result

                    let tcState = priorTcInfo.tcState

                    let sink = priorTcInfo.sink.Head // TODO: don't use head

                    let tcResolutions = sink.GetResolutions()
                    let tcSymbolUses = sink.GetSymbolUses()
                    let tcOpenDeclarations = sink.GetOpenDeclarations()

                    let tcDependencyFiles = [] // TODO add as a set to TcIntermediate

                    let tcDiagnostics =
                        seq {
                            yield! priorTcInfo.TcDiagnostics

                        //for x in tcIntermediate.tcDiagnosticsRev do
                        //    yield! x
                        }

                    let diagnosticsOptions = bootstrapInfo.TcConfig.diagnosticsOptions

                    let tcDiagnostics =
                        DiagnosticHelpers.CreateDiagnostics(
                            diagnosticsOptions,
                            false,
                            fileName,
                            tcDiagnostics,
                            suggestNamesForErrors,
                            bootstrapInfo.TcConfig.flatErrors
                        )

                    let tcDiagnostics = [| yield! creationDiags; yield! tcDiagnostics |]

                    let loadClosure = None // TODO: script support

                    let typedResults =
                        FSharpCheckFileResults.Make(
                            fileName,
                            projectSnapshot.ProjectFileName,
                            bootstrapInfo.TcConfig,
                            bootstrapInfo.TcGlobals,
                            projectSnapshot.IsIncompleteTypeCheckEnvironment,
                            None,
                            projectSnapshot.ToOptions(),
                            Array.ofList tcDependencyFiles,
                            creationDiags,
                            parseResults.Diagnostics,
                            tcDiagnostics,
                            keepAssemblyContents,
                            ccuSigForFile,
                            tcState.Ccu,
                            bootstrapInfo.TcImports,
                            tcEnv.AccessRights,
                            tcResolutions,
                            tcSymbolUses,
                            tcEnv.NameEnv,
                            loadClosure,
                            checkedImplFileOpt,
                            tcOpenDeclarations
                        )

                    return (parseResults, FSharpCheckFileAnswer.Succeeded typedResults)
            }
        )

    let ComputeParseAndCheckAllFilesInProject (bootstrapInfo: BootstrapInfo) (projectSnapshot: FSharpProjectSnapshot) =
        caches.ParseAndCheckAllFilesInProject.Get(
            projectSnapshot.Key,
            async {
                use _ =
                    Activity.start
                        "ComputeParseAndCheckAllFilesInProject"
                        [| Activity.Tags.project, projectSnapshot.ProjectFileName |> Path.GetFileName |]

                let! parsedInputs =
                    bootstrapInfo.SourceFiles
                    |> Seq.map (ComputeParseFile bootstrapInfo)
                    |> Async.Parallel

                let! graph, dependencyFiles =
                    ComputeDependencyGraphForProject bootstrapInfo.TcConfig projectSnapshot (parsedInputs |> Array.map p13)

                return!
                    processTypeCheckingGraph
                        graph
                        (processGraphNode projectSnapshot bootstrapInfo parsedInputs dependencyFiles true)
                        bootstrapInfo.InitialTcInfo
            }
        )

    let ComputeProjectExtras (bootstrapInfo: BootstrapInfo) (projectSnapshot: FSharpProjectSnapshot) =
        caches.ProjectExtras.Get(
            projectSnapshot.WithoutImplFilesThatHaveSignatures.Key,
            async {
                let! results, finalInfo = ComputeParseAndCheckAllFilesInProject bootstrapInfo projectSnapshot

                let assemblyName = bootstrapInfo.AssemblyName
                let tcConfig = bootstrapInfo.TcConfig
                let tcGlobals = bootstrapInfo.TcGlobals

                let results = results |> Seq.sortBy fst |> Seq.map snd |> Seq.toList

                // Finish the checking
                let (_tcEnvAtEndOfLastFile, topAttrs, _mimpls, _), tcState =
                    CheckMultipleInputsFinish(results, finalInfo.tcState)

                let tcState, _, ccuContents = CheckClosedInputSetFinish([], tcState)

                let generatedCcu = tcState.Ccu.CloneWithFinalizedContents(ccuContents)

                // Compute the identity of the generated assembly based on attributes, options etc.
                // Some of this is duplicated from fsc.fs
                let ilAssemRef =
                    let publicKey =
                        try
                            let signingInfo = ValidateKeySigningAttributes(tcConfig, tcGlobals, topAttrs)

                            match GetStrongNameSigner signingInfo with
                            | None -> None
                            | Some s -> Some(PublicKey.KeyAsToken(s.PublicKey))
                        with exn ->
                            errorRecoveryNoRange exn
                            None

                    let locale =
                        TryFindFSharpStringAttribute
                            tcGlobals
                            (tcGlobals.FindSysAttrib "System.Reflection.AssemblyCultureAttribute")
                            topAttrs.assemblyAttrs

                    let assemVerFromAttrib =
                        TryFindFSharpStringAttribute
                            tcGlobals
                            (tcGlobals.FindSysAttrib "System.Reflection.AssemblyVersionAttribute")
                            topAttrs.assemblyAttrs
                        |> Option.bind (fun v ->
                            try
                                Some(parseILVersion v)
                            with _ ->
                                None)

                    let ver =
                        match assemVerFromAttrib with
                        | None -> tcConfig.version.GetVersionInfo(tcConfig.implicitIncludeDir)
                        | Some v -> v

                    ILAssemblyRef.Create(assemblyName, None, publicKey, false, Some ver, locale)

                let assemblyDataResult =
                    try
                        // Assemblies containing type provider components can not successfully be used via cross-assembly references.
                        // We return 'None' for the assembly portion of the cross-assembly reference
                        let hasTypeProviderAssemblyAttrib =
                            topAttrs.assemblyAttrs
                            |> List.exists (fun (Attrib (tcref, _, _, _, _, _, _)) ->
                                let nm = tcref.CompiledRepresentationForNamedType.BasicQualifiedName

                                nm = typeof<Microsoft.FSharp.Core.CompilerServices.TypeProviderAssemblyAttribute>
                                    .FullName)

                        if tcState.CreatesGeneratedProvidedTypes || hasTypeProviderAssemblyAttrib then
                            ProjectAssemblyDataResult.Unavailable true
                        else
                            ProjectAssemblyDataResult.Available(
                                RawFSharpAssemblyDataBackedByLanguageService(
                                    bootstrapInfo.TcConfig,
                                    bootstrapInfo.TcGlobals,
                                    generatedCcu,
                                    bootstrapInfo.OutFile,
                                    topAttrs,
                                    bootstrapInfo.AssemblyName,
                                    ilAssemRef
                                )
                                :> IRawFSharpAssemblyData
                            )
                    with exn ->
                        errorRecoveryNoRange exn
                        ProjectAssemblyDataResult.Unavailable true

                return finalInfo, ilAssemRef, assemblyDataResult
            }
        )

    let ComputeAssemblyData (projectSnapshot: FSharpProjectSnapshot) =
        caches.AssemblyData.Get(
            projectSnapshot.WithoutImplFilesThatHaveSignatures.Key,
            async {
                match! ComputeBootstrapInfo projectSnapshot with
                | None, _ -> return ProjectAssemblyDataResult.Unavailable true
                | Some bootstrapInfo, _creationDiags ->
                    let! _, _, assemblyDataResult = ComputeProjectExtras bootstrapInfo projectSnapshot
                    return assemblyDataResult
            }
        )

    let ComputeParseAndCheckProject (projectSnapshot: FSharpProjectSnapshot) =
        caches.ParseAndCheckProject.Get(
            projectSnapshot.Key,
            async {

                match! ComputeBootstrapInfo projectSnapshot with
                | None, creationDiags ->
                    return FSharpCheckProjectResults(projectSnapshot.ProjectFileName, None, keepAssemblyContents, creationDiags, None)
                | Some bootstrapInfo, creationDiags ->

                    let! tcInfo, ilAssemRef, assemblyDataResult = ComputeProjectExtras bootstrapInfo projectSnapshot

                    let diagnosticsOptions = bootstrapInfo.TcConfig.diagnosticsOptions
                    let fileName = DummyFileNameForRangesWithoutASpecificLocation

                    let topAttribs = tcInfo.topAttribs
                    let tcState = tcInfo.tcState
                    let tcEnvAtEnd = tcInfo.tcEnvAtEndOfFile
                    let tcDiagnostics = tcInfo.TcDiagnostics
                    let tcDependencyFiles = tcInfo.tcDependencyFiles

                    let tcDiagnostics =
                        DiagnosticHelpers.CreateDiagnostics(
                            diagnosticsOptions,
                            true,
                            fileName,
                            tcDiagnostics,
                            suggestNamesForErrors,
                            bootstrapInfo.TcConfig.flatErrors
                        )

                    let diagnostics = [| yield! creationDiags; yield! tcDiagnostics |]

                    let getAssemblyData () =
                        match assemblyDataResult with
                        | ProjectAssemblyDataResult.Available data -> Some data
                        | _ -> None

                    let symbolUses = tcInfo.sink |> Seq.map (fun sink -> sink.GetSymbolUses())

                    let details =
                        (bootstrapInfo.TcGlobals,
                         bootstrapInfo.TcImports,
                         tcState.Ccu,
                         tcState.CcuSig,
                         Choice2Of2(async.Return symbolUses),
                         topAttribs,
                         getAssemblyData,
                         ilAssemRef,
                         tcEnvAtEnd.AccessRights,
                         None,
                         Array.ofList tcDependencyFiles,
                         projectSnapshot.ToOptions())

                    let results =
                        FSharpCheckProjectResults(
                            projectSnapshot.ProjectFileName,
                            Some bootstrapInfo.TcConfig,
                            keepAssemblyContents,
                            diagnostics,
                            Some details
                        )

                    return results
            }
        )

    let tryGetSink fileName (projectSnapshot: FSharpProjectSnapshot) =
        async {
            match! ComputeBootstrapInfo projectSnapshot with
            | None, _ -> return None
            | Some bootstrapInfo, _creationDiags ->

                let file = bootstrapInfo.GetFile fileName

                let! _, tcInfo = ComputeTcFile file bootstrapInfo projectSnapshot

                return tcInfo.sink |> List.tryHead |> Option.map (fun sink -> sink, bootstrapInfo)
        }

    let ComputeSemanticClassification (fileName: string, projectSnapshot: FSharpProjectSnapshot) =
        caches.SemanticClassification.Get(
            projectSnapshot.FileKey fileName,
            async {
                use _ =
                    Activity.start "ComputeSemanticClassification" [| Activity.Tags.fileName, fileName |> Path.GetFileName |]

                let! sinkOpt = tryGetSink fileName projectSnapshot

                return
                    sinkOpt
                    |> Option.bind (fun (sink, bootstrapInfo) ->
                        let sResolutions = sink.GetResolutions()

                        let semanticClassification =
                            sResolutions.GetSemanticClassification(
                                bootstrapInfo.TcGlobals,
                                bootstrapInfo.TcImports.GetImportMap(),
                                sink.GetFormatSpecifierLocations(),
                                None
                            )

                        let sckBuilder = SemanticClassificationKeyStoreBuilder()
                        sckBuilder.WriteAll semanticClassification

                        sckBuilder.TryBuildAndReset())
                    |> Option.map (fun sck -> sck.GetView())
            }
        )

    let ComputeItemKeyStore (fileName: string, projectSnapshot: FSharpProjectSnapshot) =
        caches.ItemKeyStore.Get(
            projectSnapshot.FileKey fileName,
            async {
                use _ =
                    Activity.start "ComputeItemKeyStore" [| Activity.Tags.fileName, fileName |> Path.GetFileName |]

                let! sinkOpt = tryGetSink fileName projectSnapshot

                return
                    sinkOpt
                    |> Option.bind (fun (sink, _) ->
                        let sResolutions = sink.GetResolutions()

                        let builder = ItemKeyStoreBuilder()

                        let preventDuplicates =
                            HashSet(
                                { new IEqualityComparer<struct (pos * pos)> with
                                    member _.Equals((s1, e1): struct (pos * pos), (s2, e2): struct (pos * pos)) =
                                        Position.posEq s1 s2 && Position.posEq e1 e2

                                    member _.GetHashCode o = o.GetHashCode()
                                }
                            )

                        sResolutions.CapturedNameResolutions
                        |> Seq.iter (fun cnr ->
                            let r = cnr.Range

                            if preventDuplicates.Add struct (r.Start, r.End) then
                                builder.Write(cnr.Range, cnr.Item))

                        builder.TryBuildAndReset())
            }
        )

    member _.ParseFile(fileName, projectSnapshot: FSharpProjectSnapshot, _userOpName) =
        async {
            //use _ =
            //    Activity.start "ParseFile" [| Activity.Tags.fileName, fileName |> Path.GetFileName |]

            match! ComputeBootstrapInfo projectSnapshot with
            | None, creationDiags -> return emptyParseResult fileName creationDiags
            | Some bootstrapInfo, creationDiags ->
                let! parseResult, _ = getParseResult bootstrapInfo creationDiags fileName
                return parseResult
        }

    member _.ParseAndCheckFileInProject(fileName: string, projectSnapshot: FSharpProjectSnapshot, userOpName: string) =
        ignore userOpName
        ComputeParseAndCheckFileInProject fileName projectSnapshot

    member _.FindReferencesInFile(fileName: string, projectSnapshot: FSharpProjectSnapshot, symbol: FSharpSymbol, userOpName: string) =
        ignore userOpName

        async {
            match! ComputeItemKeyStore(fileName, projectSnapshot) with
            | None -> return Seq.empty
            | Some itemKeyStore -> return itemKeyStore.FindAll symbol.Item
        }

    member _.GetAssemblyData(projectSnapshot: FSharpProjectSnapshot, _userOpName) =
        ComputeAssemblyData projectSnapshot |> NodeCode.AwaitAsync

    member _.Caches = caches

    interface IBackgroundCompiler with

        member this.BeforeBackgroundFileCheck: IEvent<string * FSharpProjectOptions> =
            backgroundCompiler.BeforeBackgroundFileCheck

        member this.CheckFileInProject
            (
                parseResults: FSharpParseFileResults,
                fileName: string,
                fileVersion: int,
                sourceText: ISourceText,
                options: FSharpProjectOptions,
                userOpName: string
            ) : NodeCode<FSharpCheckFileAnswer> =
            node {
                let! snapshot =
                    FSharpProjectSnapshot.FromOptions(options, fileName, fileVersion, sourceText)
                    |> NodeCode.AwaitAsync

                ignore parseResults
                let! _, result = this.ParseAndCheckFileInProject(fileName, snapshot, userOpName) |> NodeCode.AwaitAsync
                return result
            }

        member this.CheckFileInProjectAllowingStaleCachedResults
            (
                parseResults: FSharpParseFileResults,
                fileName: string,
                fileVersion: int,
                sourceText: ISourceText,
                options: FSharpProjectOptions,
                userOpName: string
            ) : NodeCode<FSharpCheckFileAnswer option> =
            node {
                let! snapshot =
                    FSharpProjectSnapshot.FromOptions(options, fileName, fileVersion, sourceText)
                    |> NodeCode.AwaitAsync

                ignore parseResults
                let! _, result = this.ParseAndCheckFileInProject(fileName, snapshot, userOpName) |> NodeCode.AwaitAsync
                return Some result
            }

        member _.ClearCache(options: seq<FSharpProjectOptions>, userOpName: string) : unit =
            backgroundCompiler.ClearCache(options, userOpName)

        member _.ClearCaches() : unit = backgroundCompiler.ClearCaches()
        member _.DownsizeCaches() : unit = backgroundCompiler.DownsizeCaches()

        member _.FileChecked: IEvent<string * FSharpProjectOptions> =
            backgroundCompiler.FileChecked

        member _.FileParsed: IEvent<string * FSharpProjectOptions> =
            backgroundCompiler.FileParsed

        member this.FindReferencesInFile
            (
                fileName: string,
                options: FSharpProjectOptions,
                symbol: FSharpSymbol,
                canInvalidateProject: bool,
                userOpName: string
            ) : NodeCode<seq<range>> =
            node {
                ignore canInvalidateProject
                let! snapshot = FSharpProjectSnapshot.FromOptions options |> NodeCode.AwaitAsync
                return! this.FindReferencesInFile(fileName, snapshot, symbol, userOpName) |> NodeCode.AwaitAsync
            }

        member this.FindReferencesInFile(fileName, projectSnapshot, symbol, userOpName) =
            node {
                return! this.FindReferencesInFile(fileName, projectSnapshot, symbol, userOpName) |> NodeCode.AwaitAsync
            }

        member _.FrameworkImportsCache: FrameworkImportsCache =
            backgroundCompiler.FrameworkImportsCache

        member this.GetAssemblyData(options: FSharpProjectOptions, userOpName: string) : NodeCode<ProjectAssemblyDataResult> =
            node {
                let! snapshot = FSharpProjectSnapshot.FromOptions options |> NodeCode.AwaitAsync
                return! this.GetAssemblyData(snapshot, userOpName)
            }

        member this.GetAssemblyData(projectSnapshot: FSharpProjectSnapshot, userOpName: string) : NodeCode<ProjectAssemblyDataResult> =
            this.GetAssemblyData(projectSnapshot, userOpName)

        member this.GetBackgroundCheckResultsForFileInProject
            (
                fileName: string,
                options: FSharpProjectOptions,
                userOpName: string
            ) : NodeCode<FSharpParseFileResults * FSharpCheckFileResults> =
            node {
                let! snapshot = FSharpProjectSnapshot.FromOptions options |> NodeCode.AwaitAsync
                

                match! this.ParseAndCheckFileInProject(fileName, snapshot, userOpName) |> NodeCode.AwaitAsync with
                | parseResult, FSharpCheckFileAnswer.Succeeded checkResult -> return parseResult, checkResult
                | parseResult, FSharpCheckFileAnswer.Aborted -> return parseResult, FSharpCheckFileResults.MakeEmpty(fileName, [||], true)
            }

        member this.GetBackgroundParseResultsForFileInProject
            (
                fileName: string,
                options: FSharpProjectOptions,
                userOpName: string
            ) : NodeCode<FSharpParseFileResults> =
            node {
                let! snapshot = FSharpProjectSnapshot.FromOptions options |> NodeCode.AwaitAsync
                return! this.ParseFile(fileName, snapshot, userOpName) |> NodeCode.AwaitAsync
            }

        member this.GetCachedCheckFileResult
            (
                builder: IncrementalBuilder,
                fileName: string,
                sourceText: ISourceText,
                options: FSharpProjectOptions
            ) : NodeCode<(FSharpParseFileResults * FSharpCheckFileResults) option> =
            node {
                ignore builder

                let! snapshot =
                    FSharpProjectSnapshot.FromOptions(options, fileName, 1, sourceText)
                    |> NodeCode.AwaitAsync


                match! this.ParseAndCheckFileInProject(fileName, snapshot, "GetCachedCheckFileResult") |> NodeCode.AwaitAsync with
                | parseResult, FSharpCheckFileAnswer.Succeeded checkResult -> return Some(parseResult, checkResult)
                | _, FSharpCheckFileAnswer.Aborted -> return None
            }

        member this.GetProjectOptionsFromScript
            (
                fileName: string,
                sourceText: ISourceText,
                previewEnabled: bool option,
                loadedTimeStamp: DateTime option,
                otherFlags: string array option,
                useFsiAuxLib: bool option,
                useSdkRefs: bool option,
                sdkDirOverride: string option,
                assumeDotNetFramework: bool option,
                optionsStamp: int64 option,
                userOpName: string
            ) : Async<FSharpProjectOptions * FSharpDiagnostic list> =
            backgroundCompiler.GetProjectOptionsFromScript(
                fileName,
                sourceText,
                previewEnabled,
                loadedTimeStamp,
                otherFlags,
                useFsiAuxLib,
                useSdkRefs,
                sdkDirOverride,
                assumeDotNetFramework,
                optionsStamp,
                userOpName
            )

        member this.GetSemanticClassificationForFile(fileName: string, snapshot: FSharpProjectSnapshot, userOpName: string) =
            node {
                ignore userOpName
                return! ComputeSemanticClassification(fileName, snapshot) |> NodeCode.AwaitAsync
            }

        member this.GetSemanticClassificationForFile
            (
                fileName: string,
                options: FSharpProjectOptions,
                userOpName: string
            ) : NodeCode<EditorServices.SemanticClassificationView option> =
            node {
                ignore userOpName
                let! snapshot = FSharpProjectSnapshot.FromOptions options |> NodeCode.AwaitAsync
                return! ComputeSemanticClassification(fileName, snapshot) |> NodeCode.AwaitAsync
            }

        member this.InvalidateConfiguration(options: FSharpProjectOptions, userOpName: string) : unit =
            backgroundCompiler.InvalidateConfiguration(options, userOpName)

        member this.NotifyFileChanged(fileName: string, options: FSharpProjectOptions, userOpName: string) : NodeCode<unit> =
            backgroundCompiler.NotifyFileChanged(fileName, options, userOpName)

        member this.NotifyProjectCleaned(options: FSharpProjectOptions, userOpName: string) : Async<unit> =
            backgroundCompiler.NotifyProjectCleaned(options, userOpName)

        member this.ParseAndCheckFileInProject
            (
                fileName: string,
                fileVersion: int,
                sourceText: ISourceText,
                options: FSharpProjectOptions,
                userOpName: string
            ) : NodeCode<FSharpParseFileResults * FSharpCheckFileAnswer> =
            node {
                let! snapshot =
                    FSharpProjectSnapshot.FromOptions(options, fileName, fileVersion, sourceText)
                    |> NodeCode.AwaitAsync

                return! this.ParseAndCheckFileInProject(fileName, snapshot, userOpName) |> NodeCode.AwaitAsync
            }

        member this.ParseAndCheckFileInProject(fileName: string, projectSnapshot: FSharpProjectSnapshot, userOpName: string) =
            this.ParseAndCheckFileInProject(fileName, projectSnapshot, userOpName)

        member this.ParseAndCheckProject(options: FSharpProjectOptions, userOpName: string) : NodeCode<FSharpCheckProjectResults> =
            node {
                ignore userOpName
                let! snapshot = FSharpProjectSnapshot.FromOptions options |> NodeCode.AwaitAsync
                return! ComputeParseAndCheckProject snapshot |> NodeCode.AwaitAsync
            }

        member this.ParseAndCheckProject(projectSnapshot: FSharpProjectSnapshot, userOpName: string) : NodeCode<FSharpCheckProjectResults> =
            node {
                ignore userOpName
                return! ComputeParseAndCheckProject projectSnapshot |> NodeCode.AwaitAsync
            }

        member this.ParseFile(fileName, projectSnapshot, userOpName) =
            node {
                return! this.ParseFile(fileName, projectSnapshot, userOpName) |> NodeCode.AwaitAsync
            }

        member this.ParseFile
            (
                fileName: string,
                sourceText: ISourceText,
                options: FSharpParsingOptions,
                cache: bool,
                flatErrors: bool,
                userOpName: string
            ) : Async<FSharpParseFileResults> =
            backgroundCompiler.ParseFile(fileName, sourceText, options, cache, flatErrors, userOpName)

        member this.ProjectChecked: IEvent<FSharpProjectOptions> =
            backgroundCompiler.ProjectChecked

        member this.TryGetRecentCheckResultsForFile
            (
                fileName: string,
                options: FSharpProjectOptions,
                sourceText: ISourceText option,
                userOpName: string
            ) : (FSharpParseFileResults * FSharpCheckFileResults * SourceTextHash) option =
            backgroundCompiler.TryGetRecentCheckResultsForFile(fileName, options, sourceText, userOpName)
