
open System.IO

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Compiler.Diagnostics


open Newtonsoft.Json.Linq
open Newtonsoft.Json
open System


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


    let run() = 

        let file = "D:/code/fsharp-snapshots/FSharp.Compiler.ComponentTests.json"
        let snapshot = loadFromJson (File.ReadAllText(file))

        let mutable version = Unchecked.defaultof<_>
        for x in 1..1000 do
            let version' = snapshot.GetXxByteVersion() |> BitConverter.ToString
            version <- version'
        printfn "Version: %s" version

FSharpProjectSnapshotSerialization.run()