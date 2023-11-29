module FSharp.Benchmarks.ProjectSnapshotBenchmarks

open System.IO
open BenchmarkDotNet.Attributes
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Diagnostics
open FSharp.Test.ProjectGeneration
open BenchmarkDotNet.Engines
open Newtonsoft.Json.Linq
open Newtonsoft.Json
open BenchmarkDotNet.Diagnosers
open System

//open BenchmarkDotNet.Diagnostics.Windows.Configs


[<Literal>]
let FSharpCategory = "fsharp"


module internal FSharpProjectSnapshotSerialization =

    let serializeFileSnapshot (snapshot: FSharpFileSnapshot) =
        let output = JObject()
        output.Add("FileName", snapshot.FileName)
        output.Add("Version", snapshot.Version)
        output

    let serializeReferenceOnDisk (reference: ReferenceOnDisk) =
        let output = JObject()
        output.Add("Path", reference.Path)
        output.Add("LastModified", reference.LastModified)
        output

    let rec serializeReferencedProject (reference: FSharpReferencedProjectSnapshot) =
        let output = JObject()
        match reference with
        | FSharpReference (projectOutputFile, snapshot) ->
            output.Add("projectOutputFile", projectOutputFile)
            output.Add("snapshot", serializeSnapshot snapshot)
        output

    and serializeSnapshot (snapshot: FSharpProjectSnapshot) =
        
        let output = JObject()

        output.Add("ProjectFileName", snapshot.ProjectFileName)
        output.Add("ProjectId", (snapshot.ProjectId |> Option.defaultValue null |> JToken.FromObject))
        output.Add("SourceFiles", snapshot.SourceFiles |> Seq.map serializeFileSnapshot |> JArray )
        output.Add("ReferencesOnDisk", snapshot.ReferencesOnDisk |> Seq.map serializeReferenceOnDisk |> JArray )
        output.Add("OtherOptions", JArray(snapshot.OtherOptions))
        output.Add("ReferencedProjects", snapshot.ReferencedProjects |> Seq.map serializeReferencedProject |> JArray )
        output.Add("IsIncompleteTypeCheckEnvironment", snapshot.IsIncompleteTypeCheckEnvironment)
        output.Add("UseScriptResolutionRules", snapshot.UseScriptResolutionRules)
        output.Add("LoadTime", snapshot.LoadTime)
        // output.Add("UnresolvedReferences", snapshot.UnresolvedReferences)
        //output.Add("OriginalLoadReferences",
        //    snapshot.OriginalLoadReferences
        //    |> Seq.map (fun (r:Text.range, a, b) ->
        //         JArray(r.FileName, r.Start, r.End, a, b)) |> JArray)

        output.Add("Stamp", (snapshot.Stamp |> (Option.defaultValue 0) |> JToken.FromObject ))

        output

    let dumpToJson (snapshot) =

        let jObject = serializeSnapshot snapshot

        let json = jObject.ToString(Formatting.Indented)

        json
    
    let deserializeFileSnapshot (json: JObject) : FSharpFileSnapshot =
        { 
            FileName = json.Value "FileName"
            Version = json.Value "Version"
            GetSource = fun () -> failwith "Dummy File Snapshot"
        }
        
    let deserializeReferenceOnDisk (json: JObject) : ReferenceOnDisk =
        { Path = json.Value<string>("Path")
          LastModified = json.Value("LastModified") }

    let rec deserializeReferencedProject (json: JObject) : FSharpReferencedProjectSnapshot =
        match json["projectOutputFile"] with
        | :? JValue as projectOutputFile ->
            FSharpReference(projectOutputFile.Value.ToString(), deserializeSnapshot (json.Value<JObject>("snapshot")))
        | _ -> failwith "oops"

    and deserializeSnapshot (json: JObject) : FSharpProjectSnapshot =
        { ProjectFileName = json.Value<string>("ProjectFileName")
          ProjectId = match json.Value<string>("ProjectId") with null -> None | id -> Some id
          SourceFiles = json["SourceFiles"].Values<JObject>() |> Seq.map (deserializeFileSnapshot) |> Seq.toList
          ReferencesOnDisk = json["ReferencesOnDisk"].Values<JObject>() |> Seq.map deserializeReferenceOnDisk |> Seq.toList
          OtherOptions = json["OtherOptions"].Values<string>() |> Seq.toList
          ReferencedProjects = json["ReferencedProjects"].Values<JObject>() |> Seq.map deserializeReferencedProject |> Seq.toList
          IsIncompleteTypeCheckEnvironment = json.Value<bool>("IsIncompleteTypeCheckEnvironment")
          UseScriptResolutionRules = json.Value<bool>("UseScriptResolutionRules")
          LoadTime = json.Value("LoadTime")
          UnresolvedReferences = None
          OriginalLoadReferences = []
          Stamp = match json.Value("Stamp") with 0 -> None | stamp -> Some stamp
          }

    let loadFromJson (json: string) : FSharpProjectSnapshot =
        let jObject = JObject.Parse(json)
        deserializeSnapshot jObject



type Hashing =
    | None = 1
    | Md5 = 2
    | Md5Bytes = 3
    | XxHash = 4
    | XxHashBytes = 5


[<MemoryDiagnoser>]
[<ThreadingDiagnoser>]
[<BenchmarkCategory(FSharpCategory)>]
//[<SimpleJob(warmupCount=1,iterationCount=4)>]
//[<EtwProfiler(performExtraBenchmarksRun=false)>]
[<EventPipeProfiler(EventPipeProfile.CpuSampling)>]
type SnapshotVersioningBenchmark() =

    let file = "D:/code/fsharp-snapshots/FSharp.Compiler.ComponentTests.json"
    let fileChanged = "D:/code/fsharp-snapshots/FSharp.Compiler.ComponentTests-Changed.json"

    let mutable snapshot = Unchecked.defaultof<FSharpProjectSnapshot>
    let mutable snapshotChanged = Unchecked.defaultof<FSharpProjectSnapshot>

    let mutable md5Version = Unchecked.defaultof<_>
    let mutable md5VersionChanged = Unchecked.defaultof<_>

    let mutable md5ByteVersion = Unchecked.defaultof<_>
    let mutable md5ByteVersionChanged = Unchecked.defaultof<_>

    let mutable debugVersion = Unchecked.defaultof<_>
    let mutable debugVersionChanged = Unchecked.defaultof<_>

    let mutable xxVersion = Unchecked.defaultof<_>
    let mutable xxVersionChanged = Unchecked.defaultof<_>

    let mutable xxBytesVersion = Unchecked.defaultof<_>
    let mutable xxBytesVersionChanged = Unchecked.defaultof<_>

    [<ParamsAllValues>]
    member val HashingAlgorithm = Hashing.None with get,set

    [<GlobalSetup>]
    member this.Setup() =
        snapshot <- FSharpProjectSnapshotSerialization.loadFromJson (File.ReadAllText(file))
        snapshotChanged <- FSharpProjectSnapshotSerialization.loadFromJson (File.ReadAllText(fileChanged))
        debugVersion <- snapshot.GetDebugVersion()
        debugVersionChanged <- snapshotChanged.GetDebugVersion()
        md5Version <- snapshot.GetMd5Version()
        md5VersionChanged <- snapshotChanged.GetMd5Version()
        md5ByteVersion <- snapshot.GetMd5ByteVersion() |> BitConverter.ToString
        md5ByteVersionChanged <- snapshotChanged.GetMd5ByteVersion() |> BitConverter.ToString
        xxVersion <- snapshot.GetXxVersion()
        xxVersionChanged <- snapshotChanged.GetXxVersion()
        xxBytesVersion <- snapshot.GetXxByteVersion() |> BitConverter.ToString
        xxBytesVersionChanged <- snapshotChanged.GetXxByteVersion() |> BitConverter.ToString

    [<Benchmark>]
    member this.ConstructVersion() =

        match this.HashingAlgorithm with
        | Hashing.None -> 
            debugVersion <- snapshot.GetDebugVersion()
            debugVersionChanged <- snapshotChanged.GetDebugVersion()

        | Hashing.Md5 -> 
            md5Version <- snapshot.GetMd5Version()
            md5VersionChanged <- snapshotChanged.GetMd5Version()

        | Hashing.Md5Bytes -> 
            md5Version <- snapshot.GetMd5ByteVersion() |> BitConverter.ToString
            md5VersionChanged <- snapshotChanged.GetMd5ByteVersion() |> BitConverter.ToString

        | Hashing.XxHash -> 
            xxVersion <- snapshot.GetXxVersion()
            xxVersionChanged <- snapshotChanged.GetXxVersion()

        | Hashing.XxHashBytes -> 
            xxBytesVersion <- snapshot.GetXxByteVersion() |> BitConverter.ToString
            xxBytesVersionChanged <- snapshotChanged.GetXxByteVersion() |> BitConverter.ToString

        | _ -> failwith "oops"

    //[<Benchmark>]
    member this.VersionComparison() =
        
        let areEqual = 
            match this.HashingAlgorithm with
            | Hashing.None -> 
                let mutable result = true
                for i in 0..100 do
                    result <- result && debugVersion = debugVersionChanged
                result
            | Hashing.Md5 -> 
                let mutable result = true
                for i in 0..100 do
                    result <- result && md5Version = md5VersionChanged
                result
            | Hashing.XxHash -> 
                let mutable result = true
                for i in 0..100 do
                    result <- result && xxVersion = xxVersionChanged
                result
            | Hashing.XxHashBytes -> 
                let mutable result = true
                for i in 0..100 do
                    result <- result && xxBytesVersion = xxBytesVersionChanged
                result

            | _ -> failwith "oops"
        if areEqual then failwith "Versions reported as equal but they should be different"