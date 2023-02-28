﻿/// Tools for generating synthetic projects where we can model dependencies between files.
///
/// Each file in the project has a string identifier. It then contains a type and a function.
/// The function calls functions from all the files the given file depends on and returns their
/// results + it's own type in a tuple.
///
/// To model changes, we change the type name in a file which results in signatures of all the
/// dependent files also changing.
///
/// To model breaking changes we change the name of the function which will make dependent files
/// not compile.
///
/// To model changes to "private" code in a file we change the body of a second function which
/// no one calls.
///
module FSharp.Test.ProjectGeneration

open System
open System.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open Xunit
open System.Collections.Concurrent

#nowarn "57" // Experimental feature use

let private projectRoot = "test-projects"

let private defaultFunctionName = "f"


type SignatureFile =
    | No
    | AutoGenerated
    | Custom of string


type SyntheticSourceFile =
    {
        Id: string
        /// This is part of the file's type name
        PublicVersion: int
        InternalVersion: int
        DependsOn: string list
        /// Changing this makes dependent files' code invalid
        FunctionName: string
        SignatureFile: SignatureFile
        HasErrors: bool
        ExtraSource: string
        EntryPoint: bool
    }

    member this.FileName = $"File{this.Id}.fs"
    member this.SignatureFileName = $"{this.FileName}i"
    member this.TypeName = $"T{this.Id}V_{this.PublicVersion}"
    member this.ModuleName = $"Module{this.Id}"

    member this.HasSignatureFile =
        match this.SignatureFile with
        | No -> false
        | _ -> true


let sourceFile fileId deps =
    { Id = fileId
      PublicVersion = 1
      InternalVersion = 1
      DependsOn = deps
      FunctionName = defaultFunctionName
      SignatureFile = No
      HasErrors = false
      ExtraSource = ""
      EntryPoint = false }


let OptionsCache = ConcurrentDictionary()


type SyntheticProject =
    { Name: string
      ProjectDir: string
      SourceFiles: SyntheticSourceFile list
      DependsOn: SyntheticProject list
      RecursiveNamespace: bool
      OtherOptions: string list }

    static member Create(?name: string) =
        let name = defaultArg name $"TestProject_{Guid.NewGuid().ToString()[..7]}"
        let dir = Path.GetFullPath projectRoot

        { Name = name
          ProjectDir = dir ++ name
          SourceFiles = []
          DependsOn = []
          RecursiveNamespace = false
          OtherOptions = [] }

    static member Create([<ParamArray>] sourceFiles: SyntheticSourceFile[]) =
        { SyntheticProject.Create() with SourceFiles = sourceFiles |> List.ofArray }

    static member Create(name: string, [<ParamArray>] sourceFiles: SyntheticSourceFile[]) =
        { SyntheticProject.Create(name) with SourceFiles = sourceFiles |> List.ofArray }

    member this.Find fileId =
        this.SourceFiles
        |> List.tryFind (fun f -> f.Id = fileId)
        |> Option.defaultWith (fun () -> failwith $"File with ID '{fileId}' not found in project {this.Name}.")

    member this.FindInAllProjects fileId =
        this.GetAllFiles()
        |> List.tryFind (fun (_, f) -> f.Id = fileId)
        |> Option.defaultWith (fun () -> failwith $"File with ID '{fileId}' not found in any project.")

    member this.FindByPath path =
        this.SourceFiles
        |> List.tryFind (fun f -> this.ProjectDir ++ f.FileName = path)
        |> Option.defaultWith (fun () -> failwith $"File {path} not found in project {this.Name}.")

    member this.ProjectFileName = this.ProjectDir ++ $"{this.Name}.fsproj"

    member this.OutputFilename = this.ProjectDir ++ $"{this.Name}.dll"

    member this.GetProjectOptions(checker: FSharpChecker) =

        let cacheKey =
            this.GetAllFiles()
            |> List.collect (fun (p, f) ->
                [ p.Name
                  f.Id
                  if f.HasSignatureFile then
                      "s" ])

        if not (OptionsCache.ContainsKey cacheKey) then
            OptionsCache[cacheKey] <-
                use _ = Activity.start "SyntheticProject.GetProjectOptions" [ "project", this.Name ]

                let baseOptions, _ =
                    checker.GetProjectOptionsFromScript(
                        "file.fs",
                        SourceText.ofString "",
                        assumeDotNetFramework = false
                    )
                    |> Async.RunSynchronously

                {
                    ProjectFileName = this.ProjectFileName
                    ProjectId = None
                    SourceFiles =
                        [| for f in this.SourceFiles do
                               if f.HasSignatureFile then
                                   this.ProjectDir ++ f.SignatureFileName

                               this.ProjectDir ++ f.FileName |]
                    OtherOptions =
                        [| yield! baseOptions.OtherOptions
                           "--optimize+"
                           for p in this.DependsOn do
                               $"-r:{p.OutputFilename}"
                           yield! this.OtherOptions |]
                    ReferencedProjects =
                        [| for p in this.DependsOn do
                               FSharpReferencedProject.CreateFSharp(p.OutputFilename, p.GetProjectOptions checker) |]
                    IsIncompleteTypeCheckEnvironment = false
                    UseScriptResolutionRules = false
                    LoadTime = DateTime()
                    UnresolvedReferences = None
                    OriginalLoadReferences = []
                    Stamp = None }

        OptionsCache[cacheKey]

    member this.GetAllProjects() =
        [ this
          for p in this.DependsOn do
              yield! p.GetAllProjects() ]

    member this.GetAllFiles() =
        [ for f in this.SourceFiles do
              this, f
          for p in this.DependsOn do
              yield! p.GetAllFiles() ]


let getFilePath p (f: SyntheticSourceFile) = p.ProjectDir ++ f.FileName
let getSignatureFilePath p (f: SyntheticSourceFile) = p.ProjectDir ++ f.SignatureFileName


type SyntheticProject with
    member this.GetFilePath fileId = this.Find fileId |> getFilePath this
    member this.GetSignatureFilePath fileId = this.Find fileId |> getSignatureFilePath this


let private renderNamespaceModule (project: SyntheticProject) (f: SyntheticSourceFile) =
    seq {
        if project.RecursiveNamespace then
            $"namespace rec {project.Name}"
            $"module {f.ModuleName}"
        else
            $"module %s{project.Name}.{f.ModuleName}"
    } |> String.concat Environment.NewLine

let renderSourceFile (project: SyntheticProject) (f: SyntheticSourceFile) =
    seq {
        renderNamespaceModule project f

        for p in project.DependsOn do
            $"open {p.Name}"

        $"type {f.TypeName}<'a> = T{f.Id} of 'a"

        $"let {f.FunctionName} x ="

        for dep in f.DependsOn do
            $"    Module{dep}.{defaultFunctionName} x,"

        $"    T{f.Id} x"

        $"let f2 x = x + {f.InternalVersion}"

        f.ExtraSource

        if f.HasErrors then
            "let wrong = 1 + 'a'"

        if f.EntryPoint then
            "[<EntryPoint>]"
            "let main _ ="
            "   f 1 |> ignore"
            "   printfn \"Hello World!\""
            "   0"
    }
    |> String.concat Environment.NewLine

let private renderFsProj (p: SyntheticProject) =
    seq {
        """
        <Project Sdk="Microsoft.NET.Sdk">

        <PropertyGroup>
            <OutputType>Exe</OutputType>
            <TargetFramework>net7.0</TargetFramework>
        </PropertyGroup>

        <ItemGroup>
        """

        for f in p.SourceFiles do
            if f.HasSignatureFile then
                $"<Compile Include=\"{f.SignatureFileName}\" />"

            $"<Compile Include=\"{f.FileName}\" />"

        """
        </ItemGroup>
        </Project>
        """
    }
    |> String.concat Environment.NewLine

let private writeFileIfChanged path content =
    if not (File.Exists path) || File.ReadAllText(path) <> content then
        File.WriteAllText(path, content)

let private writeFile (p: SyntheticProject) (f: SyntheticSourceFile) =
    let fileName = getFilePath p f
    let content = renderSourceFile p f
    writeFileIfChanged fileName content


[<AutoOpen>]
module ProjectOperations =

    let updateFile fileId updateFunction project =
        let index = project.SourceFiles |> List.findIndex (fun file -> file.Id = fileId)

        { project with
            SourceFiles =
                project.SourceFiles
                |> List.updateAt index (updateFunction project.SourceFiles[index]) }

    let updateFileInAnyProject fileId updateFunction (rootProject: SyntheticProject) =
        let project, _ = rootProject.FindInAllProjects fileId

        if project = rootProject then
            updateFile fileId updateFunction project
        else
            let index = rootProject.DependsOn |> List.findIndex ((=) project)

            { rootProject with
                DependsOn =
                    rootProject.DependsOn
                    |> List.updateAt index (updateFile fileId updateFunction project) }

    let private counter = (Seq.initInfinite ((+) 2)).GetEnumerator()

    let updatePublicSurface f =
        counter.MoveNext() |> ignore
        { f with PublicVersion = counter.Current }

    let updateInternal f =
        counter.MoveNext() |> ignore
        { f with InternalVersion = counter.Current }

    let breakDependentFiles f = { f with FunctionName = "g" }

    let setPublicVersion n f = { f with PublicVersion = n }

    let addDependency fileId f : SyntheticSourceFile =
        { f with DependsOn = fileId :: f.DependsOn }

    let addSignatureFile f =
        { f with SignatureFile = AutoGenerated }

    let checkFile fileId (project: SyntheticProject) (checker: FSharpChecker) =
        let file = project.Find fileId
        let contents = renderSourceFile project file
        let absFileName = getFilePath project file

        checker.ParseAndCheckFileInProject(
            absFileName,
            0,
            SourceText.ofString contents,
            project.GetProjectOptions checker
        )

    let getTypeCheckResult (parseResults: FSharpParseFileResults, checkResults: FSharpCheckFileAnswer) =
        Assert.True(not parseResults.ParseHadErrors)

        match checkResults with
        | FSharpCheckFileAnswer.Aborted -> failwith "Type checking was aborted"
        | FSharpCheckFileAnswer.Succeeded checkResults -> checkResults

    let getSignature parseAndCheckResults =
        match (getTypeCheckResult parseAndCheckResults).GenerateSignature() with
        | Some s -> s.ToString()
        | None -> ""

    let expectOk parseAndCheckResults _ =
        let checkResult = getTypeCheckResult parseAndCheckResults

        if checkResult.Diagnostics.Length > 0 then
            failwith $"Expected no errors, but there were some: \n%A{checkResult.Diagnostics}"

    let expectSingleWarningAndNoErrors (warningSubString:string) parseAndCheckResults _  =
        let checkResult = getTypeCheckResult parseAndCheckResults
        let errors = checkResult.Diagnostics|> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
        if errors.Length > 0 then
            failwith $"Expected no errors, but there were some: \n%A{errors}"

        let warnings = checkResult.Diagnostics|> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Warning)
        match warnings |> Array.tryExactlyOne with
        | None -> failwith $"Expected 1 warning, but got {warnings.Length} instead: \n%A{warnings}"
        | Some w ->
            if w.Message.Contains warningSubString then
                ()
            else
                failwith $"Expected 1 warning with substring '{warningSubString}' but got %A{w}"

    let expectErrors parseAndCheckResults _ =
        let checkResult = getTypeCheckResult parseAndCheckResults

        if
            (checkResult.Diagnostics
             |> Array.where (fun d -> d.Severity = FSharpDiagnosticSeverity.Error))
                .Length = 0
        then
            failwith "Expected errors, but there were none"

    let expectSignatureChanged result (oldSignature: string, newSignature: string) =
        expectOk result ()
        Assert.NotEqual<string>(oldSignature, newSignature)

    let expectSignatureContains expected result (_oldSignature, newSignature) =
        expectOk result ()
        Assert.Contains(expected, newSignature)

    let expectNoChanges result (oldSignature: string, newSignature: string) =
        expectOk result ()
        Assert.Equal<string>(oldSignature, newSignature)

    let expectNumberOfResults expected (results: 'a list) =
        if results.Length <> expected then
            failwith $"Found {results.Length} references but expected to find {expected}"

    let expectToFind expected (foundRanges: range seq) =
        let expected =
            expected
            |> Seq.sortBy (fun (file, _, _, _) -> file)
            |> Seq.toArray

        let actual =
            foundRanges
            |> Seq.map (fun r -> Path.GetFileName(r.FileName), r.StartLine, r.StartColumn, r.EndColumn)
            |> Seq.sortBy (fun (file, _, _, _) -> file)
            |> Seq.toArray

        Assert.Equal<(string * int * int * int)[]>(expected, actual)

    let rec saveProject (p: SyntheticProject) generateSignatureFiles checker =
        async {
            Directory.CreateDirectory(p.ProjectDir) |> ignore

            for ref in p.DependsOn do
                do! saveProject ref generateSignatureFiles checker

            for i in 0 .. p.SourceFiles.Length - 1 do
                let file = p.SourceFiles[i]
                writeFile p file

                let signatureFileName = p.ProjectDir ++ file.SignatureFileName

                match file.SignatureFile with
                | AutoGenerated when generateSignatureFiles ->
                    let project = { p with SourceFiles = p.SourceFiles[0..i] }
                    let! results = checkFile file.Id project checker
                    let signature = getSignature results
                    writeFileIfChanged signatureFileName signature
                | Custom signature ->
                    let signatureContent = $"{renderNamespaceModule p file}\n{signature}"
                    writeFileIfChanged signatureFileName signatureContent
                | _ -> ()

            writeFileIfChanged (p.ProjectDir ++ $"{p.Name}.fsproj") (renderFsProj p)
        }


type WorkflowContext =
    { Project: SyntheticProject
      Signatures: Map<string, string>
      Cursor: FSharpSymbolUse option }

let SaveAndCheckProject project checker =
    async {

        do! saveProject project true checker

        let! results = checker.ParseAndCheckProject(project.GetProjectOptions checker)

        if not (Array.isEmpty results.Diagnostics) then
            failwith $"Project {project.Name} failed initial check: \n%A{results.Diagnostics}"

        let! signatures =
            Async.Sequential
                [ for file in project.SourceFiles do
                      async {
                          let! result = checkFile file.Id project checker
                          let signature = getSignature result
                          return file.Id, signature
                      } ]

        return
            { Project = project
              Signatures = Map signatures
              Cursor = None }
    }

type ProjectWorkflowBuilder
    (
        initialProject: SyntheticProject,
        ?initialContext,
        ?checker: FSharpChecker,
        ?useGetSource,
        ?useChangeNotifications
    ) =

    let useGetSource = defaultArg useGetSource false
    let useChangeNotifications = defaultArg useChangeNotifications false

    let mutable latestProject = initialProject

    let getSource filePath =
        filePath
        |> latestProject.FindByPath
        |> renderSourceFile latestProject
        |> SourceText.ofString
        |> Some

    let checker =
        defaultArg
            checker
            (FSharpChecker.Create(
                keepAllBackgroundSymbolUses = false,
                enableBackgroundItemKeyStoreAndSemanticClassification = true,
                enablePartialTypeChecking = true,
                captureIdentifiersWhenParsing = true,
                documentSource = (if useGetSource then DocumentSource.Custom getSource else DocumentSource.FileSystem)
            ))

    let mapProjectAsync f workflow =
        async {
            let! ctx = workflow
            let! project = f ctx.Project
            latestProject <- project
            return { ctx with Project = project }
        }

    let mapProject f = mapProjectAsync (f >> async.Return)

    /// Creates a ProjectWorkflowBuilder which will already have the project
    /// saved and checked so time won't be spent on that.
    /// Also the project won't be deleted after the computation expression is evaluated
    member this.CreateBenchmarkBuilder() =
        let ctx = this.Yield() |> Async.RunSynchronously

        ProjectWorkflowBuilder(
            ctx.Project,
            ctx,
            useGetSource = useGetSource,
            useChangeNotifications = useChangeNotifications
        )

    member this.Checker = checker

    member this.Yield _ =
        match initialContext with
        | Some ctx -> async.Return ctx
        | _ -> SaveAndCheckProject initialProject checker

    member this.DeleteProjectDir() =
        if Directory.Exists initialProject.ProjectDir then
            Directory.Delete(initialProject.ProjectDir, true)

    member this.Run(workflow: Async<WorkflowContext>) =
        try
            Async.RunSynchronously workflow
        finally
            if initialContext.IsNone then
                this.DeleteProjectDir()

    /// Change contents of given file using `processFile` function.
    /// Does not save the file to disk.
    [<CustomOperation "updateFile">]
    member this.UpdateFile(workflow: Async<WorkflowContext>, fileId: string, processFile) =
        workflow
        |> mapProject (updateFileInAnyProject fileId processFile)
        |> mapProjectAsync (fun project ->
            async {
                use _ =
                    Activity.start "ProjectWorkflowBuilder.UpdateFile" [ Activity.Tags.project, project.Name; "fileId", fileId ]

                if useChangeNotifications then
                    let project, file = project.FindInAllProjects fileId
                    let filePath = project.ProjectDir ++ file.FileName
                    do! checker.NotifyFileChanged(filePath, project.GetProjectOptions checker)

                return project
            })

    /// Add a file above given file in the project.
    [<CustomOperation "addFileAbove">]
    member this.AddFileAbove(workflow: Async<WorkflowContext>, addAboveId: string, newFile) =
        workflow
        |> mapProject (fun project ->
            let index =
                project.SourceFiles
                |> List.tryFindIndex (fun f -> f.Id = addAboveId)
                |> Option.defaultWith (fun () -> failwith $"File {addAboveId} not found")

            { project with SourceFiles = project.SourceFiles |> List.insertAt index newFile })

    /// Remove a file from the project. The file is not deleted from disk.
    [<CustomOperation "removeFile">]
    member this.RemoveFile(workflow: Async<WorkflowContext>, fileId: string) =
        workflow
        |> mapProject (fun project ->
            { project with SourceFiles = project.SourceFiles |> List.filter (fun f -> f.Id <> fileId) })

    /// Parse and type check given file and process the results using `processResults` function.
    [<CustomOperation "checkFile">]
    member this.CheckFile(workflow: Async<WorkflowContext>, fileId: string, processResults) =
        async {
            use _ =
                Activity.start "ProjectWorkflowBuilder.CheckFile" [ Activity.Tags.project, initialProject.Name; "fileId", fileId ]

            let! ctx = workflow
            let! results = checkFile fileId ctx.Project checker

            let oldSignature = ctx.Signatures[fileId]
            let newSignature = getSignature results

            processResults results (oldSignature, newSignature)

            return { ctx with Signatures = ctx.Signatures.Add(fileId, newSignature) }
        }

    member this.CheckFile(workflow: Async<WorkflowContext>, fileId: string, processResults) =
        async {
            use _ =
                Activity.start "ProjectWorkflowBuilder.CheckFile" [ Activity.Tags.project, initialProject.Name; "fileId", fileId ]

            let! ctx = workflow
            let! results = checkFile fileId ctx.Project checker
            let typeCheckResults = getTypeCheckResult results

            let newSignature = getSignature results

            processResults typeCheckResults

            return { ctx with Signatures = ctx.Signatures.Add(fileId, newSignature) }
        }

    /// Find a symbol using the provided range, mimicking placing a cursor on it in IDE scenarios
    [<CustomOperation "placeCursor">]
    member this.PlaceCursor(workflow: Async<WorkflowContext>, fileId, line, colAtEndOfNames, fullLine, symbolNames) =
        async {
            let! ctx = workflow
            let! results = checkFile fileId ctx.Project checker
            let typeCheckResults = getTypeCheckResult results

            let su =
                typeCheckResults.GetSymbolUseAtLocation(line, colAtEndOfNames, fullLine, symbolNames)

            if su.IsNone then
                let file = ctx.Project.Find fileId

                failwith
                    $"No symbol found in {file.FileName} at {line}:{colAtEndOfNames}\nFile contents:\n\n{renderSourceFile ctx.Project file}\n"

            return { ctx with Cursor = su }
        }

    /// Find a symbol by finding the first occurrence of the symbol name in the file
    [<CustomOperation "placeCursor">]
    member this.PlaceCursor(workflow: Async<WorkflowContext>, fileId, symbolName: string) =
        async {
            let! ctx = workflow

            let source = renderSourceFile ctx.Project (ctx.Project.Find fileId)
            let index = source.IndexOf symbolName
            let line = source |> Seq.take index |> Seq.where ((=) '\n') |> Seq.length
            let fullLine = source.Split '\n' |> Array.item line
            let colAtEndOfNames = fullLine.IndexOf symbolName + symbolName.Length

            let! results = checkFile fileId ctx.Project checker
            let typeCheckResults = getTypeCheckResult results

            let su =
                typeCheckResults.GetSymbolUseAtLocation(line + 1, colAtEndOfNames, fullLine, [symbolName])

            if su.IsNone then
                let file = ctx.Project.Find fileId

                failwith
                    $"No symbol found in {file.FileName} at {line}:{colAtEndOfNames}\nFile contents:\n\n{source}\n"

            return { ctx with Cursor = su }
        }

    /// Find all references within a single file, results are provided to the 'processResults' function
    [<CustomOperation "findAllReferencesInFile">]
    member this.FindAllReferencesInFile(workflow: Async<WorkflowContext>, fileId: string, processResults) =
        async {
            let! ctx = workflow
            let options = ctx.Project.GetProjectOptions checker

            let symbolUse =
                ctx.Cursor
                |> Option.defaultWith (fun () ->
                    failwith $"Please place cursor at a valid location via placeCursor first")

            let file = ctx.Project.Find fileId
            let absFileName = getFilePath ctx.Project file

            let! results =
                checker.FindBackgroundReferencesInFile(absFileName, options, symbolUse.Symbol, fastCheck = true)

            processResults (results |> Seq.toList)

            return ctx
        }

    /// Find all references within the project, results are provided to the 'processResults' function
    [<CustomOperation "findAllReferences">]
    member this.FindAllReferences(workflow: Async<WorkflowContext>, processResults) =
        async {
            let! ctx = workflow
            let options = ctx.Project.GetProjectOptions checker

            let symbolUse =
                ctx.Cursor
                |> Option.defaultWith (fun () ->
                    failwith $"Please place cursor at a valid location via placeCursor first")

            let! results =
                [ for f in options.SourceFiles do
                      checker.FindBackgroundReferencesInFile(f, options, symbolUse.Symbol, fastCheck = true) ]
                |> Async.Parallel

            results |> Seq.collect id |> Seq.toList |> processResults
            return ctx
        }

    /// Save given file to disk.
    [<CustomOperation "saveFile">]
    member this.SaveFile(workflow: Async<WorkflowContext>, fileId: string) =
        async {
            let! ctx = workflow
            let project, file = ctx.Project.FindInAllProjects fileId
            writeFile project file
            return ctx
        }

    /// Save all files to disk.
    [<CustomOperation "saveAll">]
    member this.SaveAll(workflow: Async<WorkflowContext>) =
        async {
            let! ctx = workflow
            do! saveProject ctx.Project false checker
            return ctx
        }

    /// Find all references to a module defined in a given file.
    /// These should only be found in files that depend on this file.
    ///
    /// Requires `enableBackgroundItemKeyStoreAndSemanticClassification` to be true in the checker.
    [<CustomOperation "findAllReferencesToModuleFromFile">]
    member this.FindAllReferencesToModuleFromFile(workflow, fileId, fastCheck, processResults) =
        async {
            let! ctx = workflow
            let! results = checkFile fileId ctx.Project checker
            let typeCheckResult = getTypeCheckResult results
            let moduleName = (ctx.Project.Find fileId).ModuleName

            let symbolUse =
                typeCheckResult.GetSymbolUseAtLocation(
                    1,
                    moduleName.Length + ctx.Project.Name.Length + 8,
                    $"module {ctx.Project.Name}.{moduleName}",
                    [ moduleName ]
                )
                |> Option.defaultWith (fun () -> failwith "no symbol use found")

            let options = ctx.Project.GetProjectOptions checker

            let! results =
                [ for f in options.SourceFiles do
                      checker.FindBackgroundReferencesInFile(f, options, symbolUse.Symbol, fastCheck = fastCheck) ]
                |> Async.Parallel

            results |> Seq.collect id |> Seq.toList |> processResults
            return ctx
        }

    [<CustomOperation "compileWithFSC">]
    member this.Compile(workflow: Async<WorkflowContext>) =
        async {
            let! ctx = workflow
            let projectOptions = ctx.Project.GetProjectOptions(checker)
            let arguments =
                [|
                    yield "fsc.exe"
                    yield! projectOptions.OtherOptions
                    yield! projectOptions.SourceFiles
                |]
            let! _diagnostics, exitCode = checker.Compile(arguments)
            if exitCode <> 0 then
                exn $"Compilation failed with exit code {exitCode}" |> raise
            return ctx
        }

/// Execute a set of operations on a given synthetic project.
/// The project is saved to disk and type checked at the start.
let projectWorkflow project = ProjectWorkflowBuilder project


type SyntheticProject with

    /// Execute a set of operations on this project.
    /// The project is saved to disk and type checked at the start.
    member this.Workflow = projectWorkflow this

    member this.WorkflowWith checker =
        ProjectWorkflowBuilder(this, checker = checker)

    /// Saves project to disk and checks it with default options. Returns the FSharpChecker that was created
    member this.SaveAndCheck() =
        this.Workflow.Yield() |> Async.RunSynchronously |> ignore
        this.Workflow.Checker