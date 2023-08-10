namespace Internal.Utilities.Collections

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

open System.Collections.Concurrent


open Microsoft.VisualStudio.FSharp.Editor.CancellableTasks
open Internal.Utilities.TaskAgent


type internal StateUpdate<'TValue> =
    | CancelRequest
    | OriginatorCanceled
    | JobCompleted of 'TValue
    | JobFailed of exn

type internal MemoizeReply<'TValue> =
    | New
    | Existing of Task<'TValue>

type MemoizeRequest<'TValue> =
    | GetOrCompute of CancellableTask<'TValue> * CancellationToken
    | Sync

type internal Job<'TValue> =
    | Running of TaskCompletionSource<'TValue> * CancellationTokenSource * CancellableTask<'TValue>
    | Completed of 'TValue

type internal JobEvent =
    | Started
    | Finished
    | Canceled
    | Evicted
    | Collected
    | Weakened
    | Strengthened
    | Failed

type internal CacheEvent =
    | Evicted
    | Collected
    | Weakened
    | Strengthened

[<StructuralEquality; NoComparison>]
type internal ValueLink<'T when 'T: not struct> =
    | Strong of 'T
    | Weak of WeakReference<'T>

type internal LruCache<'TKey, 'TVersion, 'TValue when 'TKey: equality and 'TVersion: equality and 'TValue: not struct>(keepStrongly, ?keepWeakly, ?requiredToKeep, ?event) =

    let keepWeakly = defaultArg keepWeakly 100
    let requiredToKeep = defaultArg requiredToKeep (fun _ -> false)
    let event = defaultArg event (fun _ _ -> ())

    let dictionary = Dictionary<'TKey, Dictionary<'TVersion, _>>()

    // Lists to keep track of when items were last accessed. First item is most recently accessed.
    let strongList = LinkedList<'TKey * 'TVersion * string * ValueLink<'TValue>>()
    let weakList = LinkedList<'TKey * 'TVersion * string * ValueLink<'TValue>>()

    let rec removeCollected (node: LinkedListNode<_>) =
        if node <> null then
            let key, version, label, value = node.Value
            match value with
            | Weak w ->
                let next = node.Next
                match w.TryGetTarget() with
                | false, _ ->
                    weakList.Remove node
                    dictionary[key].Remove version |> ignore
                    if dictionary[key].Count = 0 then
                        dictionary.Remove key |> ignore
                    event Collected (label, key, version)
                | _ -> ()
                removeCollected next
            | _ ->
                failwith "Illegal state, strong reference in weak list"

    let cutWeakListIfTooLong() =
        if weakList.Count > keepWeakly then
            removeCollected weakList.First

            let mutable node = weakList.Last
            while weakList.Count > keepWeakly && node <> null do
                let previous = node.Previous
                let key, version, label, _ = node.Value
                weakList.Remove node
                dictionary[key].Remove version |> ignore
                if dictionary[key].Count = 0 then
                    dictionary.Remove key |> ignore
                event Evicted (label, key, version)
                node <- previous

    let cutStrongListIfTooLong() =
        let mutable node = strongList.Last
        while strongList.Count > keepStrongly && node <> null do
            let previous = node.Previous
            match node.Value with
            | _, _, _, Strong v when requiredToKeep v -> ()
            | key, version, label, Strong v ->
                strongList.Remove node
                node.Value <- key, version, label, Weak (WeakReference<_> v)
                weakList.AddFirst node
                event Weakened (label, key, version)
            | _key, _version, _label, _ -> failwith "Invalid state, weak reference in strong list"
            node <- previous
        cutWeakListIfTooLong()

    let pushNodeToTop (node: LinkedListNode<_>) =
        match node.Value with
        | _, _, _, Strong _ ->
            strongList.AddFirst node
            cutStrongListIfTooLong()
        | _, _, _, Weak _ ->
            failwith "Invalid operation, pusing weak reference to strong list"

    let pushValueToTop key version label value =
        let node = strongList.AddFirst(value=(key, version, label, Strong value))
        cutStrongListIfTooLong()
        node

    member _.Set(key, version, label, value) =
        match dictionary.TryGetValue key with
        | true, versionDict ->

            if versionDict.ContainsKey version then
                // TODO this is normal for unversioned cache;
                // failwith "Suspicious - overwriting existing version"

                let node: LinkedListNode<_> = versionDict[version]
                match node.Value with
                | _, _, _, Strong _ -> strongList.Remove node
                | _, _, _, Weak _ ->
                    weakList.Remove node
                    event Strengthened (label, key, version)

                node.Value <- key, version, label, Strong value
                pushNodeToTop node

            else
                let node = pushValueToTop key version label value
                versionDict[version] <- node
                // weaken all other versions
                for otherVersion in versionDict.Keys do
                    if otherVersion <> version then
                        let node = versionDict[otherVersion]
                        match node.Value with
                        | _, _, _, Strong value ->
                            strongList.Remove node
                            node.Value <- key, otherVersion, label, Weak (WeakReference<_> value)
                            weakList.AddFirst node
                            event Weakened (label, key, otherVersion)
                        | _, _, _, Weak _ -> ()

        | false, _ ->
            let node = pushValueToTop key version label value
            dictionary[key] <- Dictionary()
            dictionary[key][version] <- node

    member _.TryGet(key, version) =

        match dictionary.TryGetValue key with
        | false, _ -> None
        | true, versionDict ->
            match versionDict.TryGetValue version with
            | false, _ -> None
            | true, node ->
                match node.Value with
                | _, _, _, Strong v ->
                    strongList.Remove node
                    pushNodeToTop node
                    Some v

                | _, _, label, Weak w ->
                    match w.TryGetTarget() with
                    | true, value ->
                        weakList.Remove node
                        let node = pushValueToTop key version label value
                        event Strengthened (label, key, version)
                        versionDict[version] <- node
                        Some value
                    | _ ->
                        weakList.Remove node
                        versionDict.Remove version |> ignore
                        if versionDict.Count = 0 then
                            dictionary.Remove key |> ignore
                        event Collected (label, key, version)
                        None

    member _.Remove(key, version) =
        match dictionary.TryGetValue key with
        | false, _ -> ()
        | true, versionDict ->
            match versionDict.TryGetValue version with
            | true, node ->
                versionDict.Remove version |> ignore
                if versionDict.Count = 0 then
                    dictionary.Remove key |> ignore
                match node.Value with
                | _, _, _, Strong _ -> strongList.Remove node
                | _, _, _, Weak _ -> weakList.Remove node
            | _ -> ()

    //member this.Set(key, value) =
    //    this.Set(key, Unchecked.defaultof<_>, value)

    //member this.TryGet(key) =
    //    this.TryGet(key, Unchecked.defaultof<_>)

    //member this.Remove(key) =
    //    this.Remove(key, Unchecked.defaultof<_>)


type internal ICacheKey<'TKey, 'TVersion> =
    abstract member GetKey: unit -> 'TKey
    abstract member GetVersion: unit -> 'TVersion
    abstract member GetLabel: unit -> string

type private KeyData<'TKey, 'TVersion> =
    { Label: string; Key: 'TKey; Version: 'TVersion }

module internal Md5Hasher =

    let private md5 = System.Security.Cryptography.MD5.Create()

    let empty: byte array = Array.empty

    let addString (s: string) (bytes: byte array) =
        let sbytes = System.Text.Encoding.UTF8.GetBytes(s)
        Array.append bytes sbytes |> md5.ComputeHash

    let addBytes (bytes: byte array) (bytes2: byte array) =
        Array.append bytes bytes2 |> md5.ComputeHash

    let addInt (i: int) (bytes: byte array) =
        let bytes2 = BitConverter.GetBytes(i)
        Array.append bytes bytes2 |> md5.ComputeHash

    let addList (list: 'T list) (bytes: byte array) (f: 'T -> byte array) =
        let bytes2 = list |> List.map f |> Array.concat
        Array.append bytes bytes2 |> md5.ComputeHash

    let addStrings (strings: string seq) (bytes: byte array) =
        let bytes2 = strings |> Seq.map System.Text.Encoding.UTF8.GetBytes |> Array.concat
        Array.append bytes bytes2 |> md5.ComputeHash

    let addBytes' (bytes': byte array seq) (bytes: byte array) =
        let bytes2 = bytes' |> Array.concat
        Array.append bytes bytes2 |> md5.ComputeHash

    let addKey (key: ICacheKey<byte array, _>) (bytes: byte array) =
        let bytes2 = key.GetKey()
        Array.append bytes bytes2 |> md5.ComputeHash

    let addKeys<'a, 'b when 'a :> ICacheKey<byte array, 'b>> (keys: 'a seq) (bytes: byte array) =
        let bytes2 = keys |> Seq.map (fun x -> x.GetKey()) |> Array.concat
        Array.append bytes bytes2 |> md5.ComputeHash

    let addVersion (version: ICacheKey<_, byte array>) (bytes: byte array) =
        let bytes2 = version.GetVersion()
        Array.append bytes bytes2 |> md5.ComputeHash

    let addVersions<'a, 'b when 'a :> ICacheKey<'b, byte array>> (versions: 'a seq) (bytes: byte array) =
        let bytes2 = versions |> Seq.map (fun x -> x.GetVersion()) |> Array.concat
        Array.append bytes bytes2 |> md5.ComputeHash

    let addBool (b: bool) (bytes: byte array) =
        let bytes2 = BitConverter.GetBytes(b)
        Array.append bytes bytes2 |> md5.ComputeHash
    

type internal AsyncMemoize<'TKey, 'TVersion, 'TValue when 'TVersion: equality>(?keepStrongly, ?keepWeakly, ?logEvent: (string -> JobEvent * (string * 'TVersion) -> unit), ?name: string) =

    let name = defaultArg name "N/A"

    let cache =
        LruCache<string, 'TVersion, Job<'TValue>>(
            keepStrongly = defaultArg keepStrongly 100,
            keepWeakly = defaultArg keepWeakly 200,
            requiredToKeep = (function Running _ -> true | _ -> false),
            event = (function
                | Evicted -> (fun k -> logEvent |> Option.iter (fun x -> x name (JobEvent.Evicted, k)))
                | Collected -> (fun k -> logEvent |> Option.iter (fun x -> x name (JobEvent.Collected, k)))
                | Weakened -> (fun k -> logEvent |> Option.iter (fun x -> x name (JobEvent.Weakened, k)))
                | Strengthened -> (fun k -> logEvent |> Option.iter (fun x -> x name (JobEvent.Strengthened, k)))))

    //let tok = obj ()

    //let cache =
    //    MruCache<_, 'TKey, Job<'TValue>>(
    //        keepStrongly = 3,
    //        keepMax = 5,
    //        areSame = (fun (x, y) -> x = y),
    //        requiredToKeep =
    //            function
    //            | Running _ -> true
    //            | _ -> false
    //    )

    let requestCounts = Dictionary<KeyData<_, _>, int>()
    let cancellationRegistrations = Dictionary<_, _>()

    let saveRegistration key registration =
        cancellationRegistrations[key] <-
            match cancellationRegistrations.TryGetValue key with
            | true, registrations -> registration::registrations
            | _ -> [registration]

    let cancelRegistration key =
        match cancellationRegistrations.TryGetValue key with
        | true, registrations ->
            for r: CancellationTokenRegistration in registrations do
                r.Dispose()
            cancellationRegistrations.Remove key |> ignore
        | _ -> ()

    let incrRequestCount key =
        requestCounts[key] <-
            if requestCounts.ContainsKey key then
                requestCounts[key] + 1
            else
                1

    let decrRequestCount key =
        if requestCounts.ContainsKey key then
            requestCounts[key] <- requestCounts[key] - 1

    let log (eventType, keyData: KeyData<_, _>) =
        logEvent |> Option.iter (fun x -> x name (eventType, (keyData.Key, keyData.Version)))

    let gate = obj()

    let processRequest post (key: KeyData<_, _>, msg) =

        lock gate (fun () ->

            let cached = cache.TryGet (key.Key, key.Version)

            // System.Diagnostics.Trace.TraceInformation $"[{key}] GetOrCompute {cached}"

            match msg, cached with
            | Sync, _ -> New
            | GetOrCompute _, Some (Completed result) -> Existing (Task.FromResult result)
            | GetOrCompute(_, ct), Some (Running (tcs, _, _)) ->
                incrRequestCount key

                ct.Register(fun _ ->
                    let _name = name
                    post (key, CancelRequest))
                    |> saveRegistration key

                Existing tcs.Task

            | GetOrCompute(computation, ct), None ->
                incrRequestCount key

                ct.Register(fun _ ->
                    let _name = name
                    post (key, OriginatorCanceled))
                    |> saveRegistration key

                cache.Set(key.Key, key.Version, key.Label, (Running(TaskCompletionSource(), (new CancellationTokenSource()), computation)))

                New
        )

    let processStateUpdate post (key: KeyData<_, _>, action: StateUpdate<_>) =

        lock gate (fun () ->


        let cached = cache.TryGet (key.Key, key.Version)

        // System.Diagnostics.Trace.TraceInformation $"[{key}] {action} {cached}"

        match action, cached with

        | OriginatorCanceled, Some (Running (tcs, cts, computation)) ->

            decrRequestCount key
            if requestCounts[key] < 1 then
                cancelRegistration key
                cts.Cancel()
                tcs.TrySetCanceled() |> ignore
                cache.Remove (key.Key, key.Version)
                requestCounts.Remove key |> ignore
                log (Canceled, key)

            else
                // We need to restart the computation
                Task.Run(fun () ->
                    task {
                        try
                            log (Started, key)
                            let! result = computation cts.Token
                            post (key, (JobCompleted result))
                        with
                        | :? OperationCanceledException ->
                            post (key, CancelRequest)
                        | ex ->
                            post (key, (JobFailed ex))
                    }, cts.Token) |> ignore

        | OriginatorCanceled, None -> ()

            // Shouldn't happen, unless we allow evicting Running jobs from cache

        | CancelRequest, Some (Running (tcs, cts, _c)) ->

            decrRequestCount key
            if requestCounts[key] < 1 then
                cancelRegistration key
                cts.Cancel()
                tcs.TrySetCanceled() |> ignore
                cache.Remove (key.Key, key.Version)
                requestCounts.Remove key |> ignore
                log (Canceled, key)

        | CancelRequest, None
        | CancelRequest, Some (Completed _) -> ()

        | JobFailed ex, Some (Running (tcs, _cts, _c)) ->
            // TODO: should we restart if there are more requests?
            cancelRegistration key
            cache.Remove (key.Key, key.Version)
            requestCounts.Remove key |> ignore
            log (Failed, key)
            tcs.TrySetException ex |> ignore

        | JobCompleted result, Some (Running (tcs, _cts, _c)) ->
            cancelRegistration key
            cache.Set(key.Key, key.Version, key.Label, (Completed result))
            requestCounts.Remove key |> ignore
            log (Finished, key)
            tcs.SetResult result

        // Job can't be evicted from cache while it's running because then subsequent requesters would be waiting forever
        // ???
        | JobFailed _, None  -> ()

        //| OriginatorCanceled, None
        | JobCompleted _, None ->
            //failwith "Invalid state: Running job missing in cache"
            ()

        | JobFailed ex, Some (Completed _job) ->
            //failwith $"Invalid state: Failed Completed job \n%A{ex}"
            ignore ex

        | JobCompleted _result, Some (Completed _job) ->
            //failwith "Invalid state: Double-Completed job"
            ()

        | OriginatorCanceled, Some (Completed _result) ->
            //failwith "Invalid state: Canceled Completed job"
            ()

        )

    //let agent =
    //    new TaskAgent<'TKey * MemoizeRequest<'TValue>, 'TKey * StateUpdate<'TValue>, MemoizeReply<'TValue>>(processRequest, processStateUpdate)

    let rec post msg =
        Task.Run(fun () -> processStateUpdate post msg) |> ignore

    member _.Get(key: ICacheKey<_, _>, computation) =

        let key = { Label = key.GetLabel(); Key = key.GetKey(); Version = key.GetVersion() }

        cancellableTask {
            let! ct = CancellableTask.getCancellationToken()

            match processRequest post (key, GetOrCompute(computation, ct)) with
            | New ->

                try
                    log (Started, key)
                    let! result = computation
                    post (key, (JobCompleted result))
                    return result
                with
                | :? TaskCanceledException
                | :? OperationCanceledException as ex ->
                    //post (key, OriginatorCanceled)
                    return raise ex
                | ex ->
                    post (key, (JobFailed ex))
                    return raise ex

            | Existing job -> return! job
        }

    //member this.Get(key, computation) =
    //    this.Get(key, Unchecked.defaultof<_>, computation)

    //member _.Get(key, computation) =
    //    ignore key
    //    computation

    member _.Sync() =

        Task.CompletedTask

        (* task {
            let! _x = agent.PostAndAwaitReply (Unchecked.defaultof<_>, (Sync))
            return ()
        } *)

