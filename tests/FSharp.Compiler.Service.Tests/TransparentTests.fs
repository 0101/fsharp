module FSharp.Compiler.Service.Tests.TransparentTests

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open NUnit.Framework

// The goal of this test is to raise awareness of how the Transparent Compiler should be behave when only the dependent files are being type-checked.

[<Test>]
let ``Establish link`` () =
    let checker = FSharpChecker.Create(useTransparentCompiler = true)

    let args = mkProjectCommandLineArgsSilent ("Project.dll", Array.empty)

    // In the sample project we have two files who are not linked to each other initially.
    let options =
        { checker.GetProjectOptionsFromCommandLineArgs("Project.fsproj", args) with
            SourceFiles = [| "A.fs"; "B.fs" |] }

    let content_Of_A =
        """
module Project.A

type X =
    {
        Y : int
    }
"""

    let content_Of_B_V1 =
        """
module Project.B

let meh _ = ()
"""

    // We start to edit the second file and hope to use a type from the first file.
    let content_Of_B_V2 =
        """
module Project.B

let meh _ : X = ()
"""

    let getFileSnapShot useV1 _ fileName =
        async {
            return
                { FileName = fileName
                  Version = if useV1 || fileName <> "B.fs" then "1" else "2"
                  GetSource =
                    fun () ->
                        task {
                            match fileName with
                            | "A.fs" -> return SourceText.ofString content_Of_A
                            | "B.fs" -> return SourceText.ofString (if useV1 then content_Of_B_V1 else content_Of_B_V2)
                            | _ -> return failwithf $"No source for %s{fileName}"
                        } }
        }

    let projectSnapShot_Initial =
        FSharpProjectSnapshot.FromOptions(options, getFileSnapShot true)
        |> Async.RunImmediate

    let unwrapCheckResult (_, checkResult) =
        match checkResult with
        | FSharpCheckFileAnswer.Aborted -> failwith "aborted"
        | FSharpCheckFileAnswer.Succeeded checkFileResults -> checkFileResults

    let initialCheckResult =
        checker.ParseAndCheckFileInProject("B.fs", projectSnapShot_Initial)
        |> Async.RunImmediate
        |> unwrapCheckResult

    Assert.True(Array.isEmpty initialCheckResult.Diagnostics)

    // Now we start to edit the second file and introduce an unresolved type

    let projectSnapShot_Subsequent =
        FSharpProjectSnapshot.FromOptions(options, getFileSnapShot false)
        |> Async.RunImmediate

    let subsequentCheckResult =
        checker.ParseAndCheckFileInProject("B.fs", projectSnapShot_Subsequent)
        |> Async.RunImmediate
        |> unwrapCheckResult

    // After the second checker.ParseAndCheckFileInProject
    // CheckOneInputWithCallback will still not have been called for A.fs
    // This is understandable, as no link was established via the AST in both cases.

    // This leads to `  B.fs(3, 13): [FS0039] The type 'X' is not defined.`
    // And my main question is how editor will resolve the missing 'X' via an editor light bulb-like action?
    // I don't know how the different editors deal with this today, but there might be a chance that they rely on all the files being type-checked.
    // If this is the case, we should discuss this with the community.
    let diagnostic =
        subsequentCheckResult.Diagnostics
        |> Array.find (fun diag -> diag.ErrorNumber = 39)

    ()
