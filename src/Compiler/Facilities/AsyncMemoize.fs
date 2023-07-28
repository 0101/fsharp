namespace Internal.Utilities.Collections

open System
open System.Collections.Generic
open System.Threading
open FSharp.Compiler.BuildGraph
open System.Threading.Tasks


type internal Action<'TKey, 'TValue> =
    | GetOrCompute of NodeCode<'TValue> * CancellationToken
    | CancelRequest
    | OriginatorCanceled
    | JobCompleted of 'TValue
    | JobFailed of exn
    | Sync

type internal MemoizeReply<'TValue> =
    | New of TaskCompletionSource<'TValue>
    | Existing of NodeCode<'TValue>

type MemoizeRequest<'TKey, 'TValue> = 'TKey * Action<'TKey, 'TValue> * AsyncReplyChannel<MemoizeReply<'TValue>>

type internal Job<'TValue> =
    | Running of TaskCompletionSource<'TValue> * CancellationTokenSource * NodeCode<'TValue>
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

type internal LruCache<'TKey, 'TValue when 'TKey: equality and 'TValue: not struct>(keepStrongly, ?keepWeakly, ?requiredToKeep, ?event) =

    let keepWeakly = defaultArg keepWeakly 100
    let requiredToKeep = defaultArg requiredToKeep (fun _ -> false)
    let event = defaultArg event (fun _ _ -> ())

    let dictionary = Dictionary<_, _>()

    // Lists to keep track of when items were last accessed. First item is most recently accessed.
    let strongList = LinkedList<'TKey * ValueLink<'TValue>>()
    let weakList = LinkedList<'TKey * ValueLink<'TValue>>()

    let rec removeCollected (node: LinkedListNode<'TKey * ValueLink<'TValue>>) =
        if node <> null then
            let k, value = node.Value
            match value with
            | Weak w ->
                let next = node.Next
                match w.TryGetTarget() with
                | false, _ ->
                    weakList.Remove node
                    dictionary.Remove k |> ignore
                    event Collected k
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

    member _.Set(key, value) = 
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

    member _.TryGet(key) = 

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
            keepStrongly = defaultArg keepStrongly 10,
            keepWeakly = defaultArg keepWeakly 10,
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

    let sendAsync (inbox: MailboxProcessor<_>) key msg =
        inbox.PostAndAsyncReply(fun rc -> key, msg, rc) |> Async.Ignore |> Async.Start

    let log event =
        logEvent |> Option.iter (fun x -> x name event)

    let agent =
        MailboxProcessor.Start(fun (inbox: MailboxProcessor<MemoizeRequest<_, _>>) ->

            let post = sendAsync inbox

            async {
                while true do
                    try
                        let _name = name

                        let! key, action, replyChannel = inbox.Receive()

                        let cached =
                            match action with
                            | Sync -> None
                            | _ -> cache.TryGet(key)

                        //System.Diagnostics.Trace.TraceInformation $"{key} {action} {cached.IsSome}"

                        match action, cached with
                        | Sync, _ -> replyChannel.Reply Unchecked.defaultof<_>
                        | GetOrCompute _, Some (Completed result) -> replyChannel.Reply (Existing (node.Return result))
                        | GetOrCompute (_, ct), Some (Running (tcs, _, _)) ->
                            incrRequestCount key

                            ct.Register(fun _ ->
                                let _name = name
                                post key CancelRequest)
                                |> saveRegistration key

                            replyChannel.Reply (Existing (tcs.Task |> NodeCode.AwaitTask))

                        | GetOrCompute (computation, ct), None ->

                            let cts = new CancellationTokenSource()

                            ct.Register(fun _ ->
                                let _name = name
                                post key OriginatorCanceled)
                                |> saveRegistration key

                            let tcs = TaskCompletionSource()

                            let wrappedComputation =
                                node {
                                    try
                                        log (Started, key)
                                        let! result = computation
                                        post key (JobCompleted result)
                                        return result
                                    with
                                    | :? TaskCanceledException
                                    | :? OperationCanceledException as ex ->
                                        post key OriginatorCanceled
                                        return raise ex
                                    | ex ->
                                        post key (JobFailed ex)
                                        return raise ex
                                }

                            cache.Set(key, (Running(tcs, cts, wrappedComputation)))

                            incrRequestCount key

                            replyChannel.Reply (New tcs)

                        | OriginatorCanceled, Some (Running (_tcs, cts, wrappedComputation)) ->
                            decrRequestCount key

                            if requestCounts[key] < 1 then
                                cts.Cancel()
                                cache.Remove key
                                requestCounts.Remove key |> ignore
                                log (Canceled, key)
                            else
                                // We need to restart the computation
                                Async.Start(
                                    wrappedComputation
                                    |> Async.AwaitNodeCode
                                    |> Async.Ignore, // Ignore the result, which will be delivered via the TaskCompletionSource from within wrappedComputation
                                    cts.Token)
                        | OriginatorCanceled, None -> ()

                         // Shouldn't happen, unless we allow evicting Running jobs from cache

                        | CancelRequest, Some (Running (_tcs, cts, _c)) ->
                            decrRequestCount key

                            if requestCounts[key] < 1 then
                                cancelRegistration key
                                cts.Cancel()
                                //cache.RemoveAnySimilar(tok, key)
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

                    with
                    | :? OperationCanceledException as e ->
                        System.Diagnostics.Trace.TraceError($"AsyncMemoize OperationCanceledException: {e.Message}")
                    | ex ->
                        let _requestCounts = requestCounts
                        System.Diagnostics.Trace.TraceError($"AsyncMemoize Exception: %A{ex}")
            })

    member _.Get(key, computation) =

        let post = sendAsync agent

        node {
            let! ct = NodeCode.CancellationToken

            match! agent.PostAndAsyncReply(fun rc -> key, (GetOrCompute(computation, ct)), rc) |> NodeCode.AwaitAsync  with
            //match agent.PostAndReply(fun rc -> key, (GetOrCompute(computation, ct)), rc) with
            | New _tcs ->

                try
                    log (Started, key)
                    let! result = computation
                    post key (JobCompleted result)
                    return result
                with
                | :? TaskCanceledException
                | :? OperationCanceledException as ex ->
                    post key OriginatorCanceled
                    return raise ex
                | ex ->
                    post key (JobFailed ex)
                    return raise ex

            | Existing job -> return! job

        }

    //member _.Get(key, computation) =
    //    ignore key
    //    computation

    member _.Sync() =
        agent.PostAndAsyncReply(fun rc -> Unchecked.defaultof<_>, (Sync), rc) |> Async.Ignore
