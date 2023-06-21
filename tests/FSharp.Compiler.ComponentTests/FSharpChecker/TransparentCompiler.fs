﻿module FSharpChecker.TransparentCompiler

open System.Collections.Concurrent
open System.Diagnostics
open FSharp.Compiler.CodeAnalysis
open Internal.Utilities.Collections
open FSharp.Compiler.CodeAnalysis.TransparentCompiler

open Xunit

open FSharp.Test.ProjectGeneration
open System.IO


[<Fact>]
let ``Use Transparent Compiler`` () =

    let size = 20

    let project =
        { SyntheticProject.Create() with
            SourceFiles = [
                sourceFile $"File%03d{0}" []
                for i in 1..size do
                    sourceFile $"File%03d{i}" [$"File%03d{i-1}"]
            ]
        }

    let first = "File001"
    let middle = $"File%03d{size / 2}"
    let last = $"File%03d{size}"

    ProjectWorkflowBuilder(project, useTransparentCompiler = true) {
        updateFile first updatePublicSurface
        checkFile first expectSignatureChanged
        checkFile last expectSignatureChanged
        updateFile middle updatePublicSurface
        checkFile last expectSignatureChanged
        addFileAbove middle (sourceFile "addedFile" [first])
        updateFile middle (addDependency "addedFile")
        checkFile middle expectSignatureChanged
        checkFile last expectSignatureChanged
    }

[<Fact>]
let ``Parallel processing`` () =

    let project = SyntheticProject.Create(
        sourceFile "A" [],
        sourceFile "B" ["A"],
        sourceFile "C" ["A"],
        sourceFile "D" ["A"],
        sourceFile "E" ["B"; "C"; "D"])

    ProjectWorkflowBuilder(project, useTransparentCompiler = true) {
        checkFile "E" expectOk
        updateFile "A" updatePublicSurface
        checkFile "E" expectSignatureChanged
    }

[<Fact>]
let ``Parallel processing with signatures`` () =

    let project = SyntheticProject.Create(
        sourceFile "A" [] |> addSignatureFile,
        sourceFile "B" ["A"] |> addSignatureFile,
        sourceFile "C" ["A"] |> addSignatureFile,
        sourceFile "D" ["A"] |> addSignatureFile,
        sourceFile "E" ["B"; "C"; "D"] |> addSignatureFile)

    let cacheEvents = ConcurrentBag<_>()

    ProjectWorkflowBuilder(project, useTransparentCompiler = true) {
        withChecker (fun checker -> checker.CacheEvent.Add cacheEvents.Add)
        checkFile "E" expectOk
        updateFile "A" updatePublicSurface
        checkFile "E" expectNoChanges
        regenerateSignature "A"
        regenerateSignature "B"
        regenerateSignature "C"
        regenerateSignature "D"
        regenerateSignature "E"
        checkFile "E" expectSignatureChanged
    }

let makeTestProject () =
    SyntheticProject.Create(
        sourceFile "First" [],
        sourceFile "Second" ["First"],
        sourceFile "Third" ["First"],
        { sourceFile "Last" ["Second"; "Third"] with EntryPoint = true })

let testWorkflow () =
    ProjectWorkflowBuilder(makeTestProject(), useTransparentCompiler = true)

[<Fact>]
let ``Edit file, check it, then check dependent file`` () =
    testWorkflow() {
        updateFile "First" breakDependentFiles
        checkFile "First" expectSignatureChanged
        checkFile "Second" expectErrors
    }

[<Fact>]
let ``Edit file, don't check it, check dependent file`` () =
    testWorkflow() {
        updateFile "First" breakDependentFiles
        checkFile "Second" expectErrors
    }

[<Fact>]
let ``Check transitive dependency`` () =
    testWorkflow() {
        updateFile "First" breakDependentFiles
        checkFile "Last" expectSignatureChanged
    }

[<Fact>]
let ``Change multiple files at once`` () =
    testWorkflow() {
        updateFile "First" (setPublicVersion 2)
        updateFile "Second" (setPublicVersion 2)
        updateFile "Third" (setPublicVersion 2)
        checkFile "Last" (expectSignatureContains "val f: x: 'a -> (ModuleFirst.TFirstV_2<'a> * ModuleSecond.TSecondV_2<'a>) * (ModuleFirst.TFirstV_2<'a> * ModuleThird.TThirdV_2<'a>) * TLastV_1<'a>")
    }

[<Fact>]
let ``Files depend on signature file if present`` () =
    let project = makeTestProject() |> updateFile "First" addSignatureFile

    ProjectWorkflowBuilder(project, useTransparentCompiler = true) {
        updateFile "First" breakDependentFiles
        saveFile "First"
        checkFile "Second" expectNoChanges
    }

[<Fact>]
let ``Signature update`` () =

    let project = SyntheticProject.Create(
        { sourceFile "First" [] with
            Source = "let f (x: int) = x"
            SignatureFile = Custom "val f: x: int -> int" },
        { sourceFile "Second" ["First"] with
            Source = "let a x = ModuleFirst.f x" })

    ProjectWorkflowBuilder(project, useTransparentCompiler = true) {
        checkFile "Second" expectOk
        updateFile "First" (fun f -> { f with SignatureFile = Custom "val f: x: string -> string" })
        checkFile "Second" expectSignatureChanged
    }

[<Fact>]
let ``Adding a file`` () =
    testWorkflow() {
        addFileAbove "Second" (sourceFile "New" [])
        updateFile "Second" (addDependency "New")
        checkFile "Last" (expectSignatureContains "val f: x: 'a -> (ModuleFirst.TFirstV_1<'a> * ModuleNew.TNewV_1<'a> * ModuleSecond.TSecondV_1<'a>) * (ModuleFirst.TFirstV_1<'a> * ModuleThird.TThirdV_1<'a>) * TLastV_1<'a>")
    }

[<Fact>]
let ``Removing a file`` () =
    testWorkflow() {
        removeFile "Second"
        checkFile "Last" expectErrors
    }

[<Fact>]
let ``Changes in a referenced project`` () =
    let library = SyntheticProject.Create("library", sourceFile "Library" [])

    let project =
        { makeTestProject() with DependsOn = [library] }
        |> updateFile "First" (addDependency "Library")

    ProjectWorkflowBuilder(project, useTransparentCompiler = true) {

        updateFile "First" updatePublicSurface
        checkFile "Last" expectOk

        updateFile "Library" updatePublicSurface
        saveFile "Library"
        checkFile "Last" expectSignatureChanged

    }

[<Fact>]
let ``We don't check files that are not depended on`` () =
    let project = SyntheticProject.Create(
        sourceFile "First" [],
        sourceFile "Second" ["First"],
        sourceFile "Third" ["First"],
        sourceFile "Last" ["Third"])

    let cacheEvents = ResizeArray()

    ProjectWorkflowBuilder(project, useTransparentCompiler = true) {
        withChecker (fun checker ->
            async {
                do! Async.Sleep 50 // wait for events from initial project check
                checker.CacheEvent.Add cacheEvents.Add
            })
        updateFile "First" updatePublicSurface
        checkFile "Last" expectOk
    } |> ignore

    let intermediateTypeChecks =
        cacheEvents
        |> Seq.choose (function
            | ("TcIntermediate", e, k) -> Some ((k :?> FSharpProjectSnapshotKey).LastFile |> fst |> Path.GetFileName, e)
            | _ -> None)
        |> Seq.groupBy fst
        |> Seq.map (fun (k, g) -> k, g |> Seq.map snd |> Seq.toList)
        |> Map

    Assert.Equal<JobEventType list>([Started; Finished], intermediateTypeChecks["FileFirst.fs"])
    Assert.Equal<JobEventType list>([Started; Finished], intermediateTypeChecks["FileThird.fs"])
    Assert.False (intermediateTypeChecks.ContainsKey "FileSecond.fs")

[<Fact>]
let ``Files that are not depended on don't invalidate cache`` () =
    let project = SyntheticProject.Create(
        sourceFile "First" [],
        sourceFile "Second" ["First"],
        sourceFile "Third" ["First"],
        sourceFile "Last" ["Third"])

    let cacheEvents = ResizeArray()

    ProjectWorkflowBuilder(project, useTransparentCompiler = true) {
        updateFile "First" updatePublicSurface
        checkFile "Last" expectOk
        withChecker (fun checker ->
            async {
                do! Async.Sleep 50 // wait for events from initial project check
                checker.CacheEvent.Add cacheEvents.Add
            })
        updateFile "Second" updatePublicSurface
        checkFile "Last" expectOk
    } |> ignore

    let intermediateTypeChecks =
        cacheEvents
        |> Seq.choose (function
            | ("TcIntermediate", e, k) -> Some ((k :?> FSharpProjectSnapshotKey).LastFile |> fst |> Path.GetFileName, e)
            | _ -> None)
        |> Seq.groupBy fst
        |> Seq.map (fun (k, g) -> k, g |> Seq.map snd |> Seq.toList)
        |> Map

    let graphConstructions =
        cacheEvents
        |> Seq.choose (function
            | ("DependencyGraph", e, k) -> Some ((k :?> (FSharpFileKey list * DependencyGraphType)) |> fst |> List.last |> fst |> Path.GetFileName, e)
            | _ -> None)
        |> Seq.groupBy fst
        |> Seq.map (fun (k, g) -> k, g |> Seq.map snd |> Seq.toList)
        |> Map

    Assert.Equal<JobEventType list>([Started; Finished], graphConstructions["FileLast.fs"])

    Assert.Equal<string * JobEventType list>([], intermediateTypeChecks |> Map.toList)

[<Fact>]
let ``Files that are not depended on don't invalidate cache part 2`` () =
    let project = SyntheticProject.Create(
        sourceFile "A" [],
        sourceFile "B" ["A"],
        sourceFile "C" ["A"],
        sourceFile "D" ["B"; "C"],
        sourceFile "E" ["C"])

    let cacheEvents = ResizeArray()

    ProjectWorkflowBuilder(project, useTransparentCompiler = true) {
        updateFile "A" updatePublicSurface
        checkFile "D" expectOk
        withChecker (fun checker ->
            async {
                do! Async.Sleep 50 // wait for events from initial project check
                checker.CacheEvent.Add cacheEvents.Add
            })
        updateFile "B" updatePublicSurface
        checkFile "E" expectOk
    } |> ignore

    let intermediateTypeChecks =
        cacheEvents
        |> Seq.choose (function
            | ("TcIntermediate", e, k) -> Some ((k :?> FSharpProjectSnapshotKey).LastFile |> fst |> Path.GetFileName, e)
            | _ -> None)
        |> Seq.groupBy fst
        |> Seq.map (fun (k, g) -> k, g |> Seq.map snd |> Seq.toList)
        |> Map

    let graphConstructions =
        cacheEvents
        |> Seq.choose (function
            | ("DependencyGraph", e, k) -> Some ((k :?> (FSharpFileKey list * DependencyGraphType)) |> fst |> List.last |> fst |> Path.GetFileName, e)
            | _ -> None)
        |> Seq.groupBy fst
        |> Seq.map (fun (k, g) -> k, g |> Seq.map snd |> Seq.toList)
        |> Map

    Assert.Equal<JobEventType list>([Started; Finished], graphConstructions["FileE.fs"])

    Assert.Equal<string * JobEventType list>([], intermediateTypeChecks |> Map.toList)


type Operation = Update | Check | Add | Remove

//[<Theory>]
//[<InlineData true>]
//[<InlineData false>]
let Fuzzing signatureFiles =
    let seed = System.Random().Next()
    let rng = System.Random(int seed)

    let fileCount = 50
    let maxDepsPerFile = 3
    let threads = 4
    let operationCount = 50

    let fileName i = sprintf $"F%03d{i}"

    let files =
        [| for i in 1 .. fileCount do
            let name = fileName i
            let deps = [
                for _ in 1 .. maxDepsPerFile do
                    if i > 1 then
                      fileName <| rng.Next(1, i) ]
            let signature = if signatureFiles then AutoGenerated else No

            { sourceFile name deps with SignatureFile = signature }
        |]

    let project = SyntheticProject.Create(files)

    let builder = ProjectWorkflowBuilder(project, useTransparentCompiler = true, autoStart = false)

    let operationProbabilities = [
        Update, 20
        Check, 20
        // Add, 2
        // Remove, 1
    ]

    let operationPicker = [|
        for op, prob in operationProbabilities do
            for _ in 1 .. prob do
                op
    |]

    let getRandomOperation () = operationPicker[rng.Next(0, operationPicker.Length)]

    let getRandomFile (project: SyntheticProject) = project.SourceFiles[rng.Next(0, project.SourceFiles.Length)].Id

    let rec fuzzer n actx = async {
        if n >= operationCount then
            return! actx
        else
            let! ctx = actx
            let project = ctx.Project
            return! fuzzer (n + 1) <|
                match getRandomOperation () with
                | Update ->
                    let file = getRandomFile project
                    builder.UpdateFile(actx, file, updatePublicSurface)
                | Check ->
                    let file = getRandomFile project
                    builder.CheckFile(actx, file, expectOk) // TODO: add timeout/cancelation
                | Add ->
                    let file = getRandomFile project
                    let newFile = sourceFile (fileName <| project.SourceFiles.Length + 1) []
                    builder.AddFileAbove(actx, file, newFile)
                | Remove ->
                    let file = getRandomFile project
                    builder.RemoveFile(actx, file)
    }

    let initialCtx = builder.Yield()

    let results =
        async {
            let! ctx = initialCtx // Save and initial check project
            return!
                [1..threads]
                |> Seq.map (fun _ -> fuzzer 0 (async.Return ctx))
                |> Async.Parallel
        }
        |> Async.RunSynchronously

    ignore results