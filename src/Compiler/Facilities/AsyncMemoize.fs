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

    let dictionary = Dictionary<'TKey, Dictionary<'TVersion, 'TValue>>()

    // Lists to keep track of when items were last accessed. First item is most recently accessed.
    let strongList = LinkedList<'TKey * 'TVersion * ValueLink<'TValue>>()
    let weakList = LinkedList<'TKey * 'TVersion * ValueLink<'TValue>>()

    let rec removeCollected (node: LinkedListNode<'TKey * 'TVersion * ValueLink<'TValue>>) =
        if node <> null then
            let key, version, value = node.Value
            match value with
            | Weak w ->
                let next = node.Next
                match w.TryGetTarget() with
                | false, _ ->
                    weakList.Remove node
                    dictionary[key].Remove version |> ignore
                    event Collected (key, version)
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
                let key = fst node.Value
                weakList.Remove node
                dictionary.Remove key |> ignore
                event Evicted key
                node <- previous

    let cutStrongListIfTooLong() =
        let mutable node = strongList.Last
        while strongList.Count > keepStrongly && node <> null do
            let previous = node.Previous
            match node.Value with
            | _, Strong v when requiredToKeep v -> ()
            | k, Strong v ->
                strongList.Remove node
                node.Value <- k, Weak (WeakReference<_> v)
                weakList.AddFirst node
                event Weakened k
            | _key, _ -> failwith "Invalid state, weak reference in strong list"
            node <- previous
        cutWeakListIfTooLong()

    let pushNodeToTop (node: LinkedListNode<_>) =
        match node.Value with
        | _, Strong _ ->
            strongList.AddFirst node
            cutStrongListIfTooLong()
        | _, Weak _ ->
            failwith "Invalid operation, pusing weak reference to strong list"

    let pushValueToTop key value =
        let node = strongList.AddFirst(value=(key, Strong value))
        cutStrongListIfTooLong()
        node

    member _.Set(key, version, value) = 
        if dictionary.ContainsKey key then
            let node: LinkedListNode<_> = dictionary[key]
            match node.Value with
            | _, Strong _ -> strongList.Remove node
            | _, Weak _ -> 
                weakList.Remove node
                event Strengthened key

            node.Value <- key, Strong value
            pushNodeToTop node

        else
            let node = pushValueToTop key value
            dictionary[key] <- node

    member _.TryGet(key, version) = 

        match dictionary.TryGetValue key with
        | true, node ->
            match node.Value with
            | _, Strong v ->
                strongList.Remove node
                pushNodeToTop node
                Some v

            | _, Weak w ->
                match w.TryGetTarget() with
                | true, v ->
                    weakList.Remove node
                    let node = pushValueToTop key v
                    event Strengthened key
                    dictionary[key] <- node
                    Some v
                | _ ->
                    weakList.Remove node
                    dictionary.Remove key |> ignore
                    event Collected key
                    None
        | _ -> None

    member _.Remove(key) =
        match dictionary.TryGetValue key with
        | true, node ->
            dictionary.Remove key |> ignore
            match node.Value with
            | _, Strong _ -> strongList.Remove node
            | _, Weak _ -> weakList.Remove node
        | _ -> ()

type internal AsyncMemoize<'TKey, 'TValue when 'TKey: equality>(?keepStrongly, ?keepWeakly, ?logEvent: (string -> JobEvent * 'TKey -> unit), ?name: string) =

    let name = defaultArg name "N/A"

    let cache =
        LruCache<'TKey, Job<'TValue>>(
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

    let requestCounts = Dictionary<'TKey, int>()
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

    let log event =
        logEvent |> Option.iter (fun x -> x name event)

    let gate = obj()

    let processRequest post (key, msg) =
        
        lock (gate) (fun () ->
                
            let cached = cache.TryGet key

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

                cache.Set(key, (Running(TaskCompletionSource(), (new CancellationTokenSource()), computation)))

                New
        )

    let processStateUpdate post (key, action: StateUpdate<_>) =

        lock gate (fun () ->


        let cached = cache.TryGet key

        // System.Diagnostics.Trace.TraceInformation $"[{key}] {action} {cached}"

        match action, cached with

        | OriginatorCanceled, Some (Running (tcs, cts, computation)) ->

            decrRequestCount key
            if requestCounts[key] < 1 then
                cancelRegistration key
                cts.Cancel()
                tcs.TrySetCanceled() |> ignore
                cache.Remove key
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
                cache.Remove key
                requestCounts.Remove key |> ignore
                log (Canceled, key)

        | CancelRequest, None
        | CancelRequest, Some (Completed _) -> ()

        | JobFailed ex, Some (Running (tcs, _cts, _c)) ->
            // TODO: should we restart if there are more requests?
            cancelRegistration key
            cache.Remove key
            requestCounts.Remove key |> ignore
            log (Failed, key)
            tcs.TrySetException ex |> ignore

        | JobCompleted result, Some (Running (tcs, _cts, _c)) ->
            cancelRegistration key
            cache.Set(key, (Completed result))
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

    member _.Get(key, computation) =

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

    //member _.Get(key, computation) =
    //    ignore key
    //    computation

    member _.Sync() =

        Task.CompletedTask

        (* task {
            let! _x = agent.PostAndAwaitReply (Unchecked.defaultof<_>, (Sync))
            return ()
        } *)

