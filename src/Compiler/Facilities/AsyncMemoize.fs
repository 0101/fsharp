namespace Internal.Utilities.Collections

open System
open System.Collections.Generic
open System.Threading
open FSharp.Compiler.BuildGraph


type internal Action<'TKey, 'TValue> =
    | GetOrCompute of NodeCode<'TValue> * CancellationToken
    | CancelRequest
    | JobCompleted of 'TValue

type MemoizeRequest<'TKey, 'TValue> = 'TKey * Action<'TKey, 'TValue> * AsyncReplyChannel<NodeCode<'TValue>>

type internal Job<'TValue> =
    | Running of NodeCode<'TValue> * CancellationTokenSource
    | Completed of NodeCode<'TValue>

type internal JobEventType =
    | Started
    | Finished
    | Canceled

[<StructuralEquality; NoComparison>]
type internal ValueLink<'T when 'T: not struct> =
    | Strong of 'T
    | Weak of WeakReference<'T>

type internal LruCache<'TKey, 'TValue when 'TKey: equality and 'TValue: not struct>(keepStrongly, ?keepWeakly, ?requiredToKeep) =

    let keepWeakly = defaultArg keepWeakly 100
    let requiredToKeep = defaultArg requiredToKeep (fun _ -> false)
    
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
                weakList.Remove node
                dictionary.Remove (fst node.Value) |> ignore
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
            | _, Weak _ -> weakList.Remove node

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
                    dictionary[key] <- node
                    Some v
                | _ ->
                    weakList.Remove node
                    dictionary.Remove key |> ignore
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

type internal AsyncMemoize<'TKey, 'TValue when 'TKey: equality>(?logEvent: (string -> JobEventType * 'TKey -> unit), ?name: string) =

    let name = defaultArg name "N/A"
    //let tok = obj ()

    let cache =
        LruCache<'TKey, Job<'TValue>>(
            keepStrongly = 20,
            keepWeakly = 100,
            //areSame = (fun (x, y) -> x = y),
            requiredToKeep =
                function
                | Running _ -> true
                | _ -> false
        )

    let requestCounts = Dictionary<'TKey, int>()

    let incrRequestCount key =
        requestCounts.[key] <-
            if requestCounts.ContainsKey key then
                requestCounts.[key] + 1
            else
                1

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

                        match action, cache.TryGet(key) with
                        | GetOrCompute _, Some (Completed job) -> replyChannel.Reply job
                        | GetOrCompute (_, ct), Some (Running (job, _)) ->
                            incrRequestCount key
                            replyChannel.Reply job
                            ct.Register(fun _ -> post key CancelRequest) |> ignore

                        | GetOrCompute (computation, ct), None ->

                            let cts = new CancellationTokenSource()

                            let startedComputation =
                                Async.StartAsTask(
                                    Async.AwaitNodeCode(
                                        node {
                                            log (Started, key)
                                            let! result = computation
                                            post key (JobCompleted result)
                                            return result
                                        }
                                    ),
                                    cancellationToken = cts.Token
                                )

                            let job = NodeCode.AwaitTask startedComputation

                            cache.Set(key, (Running(job, cts)))

                            incrRequestCount key

                            ct.Register(fun _ -> post key CancelRequest) |> ignore

                            replyChannel.Reply job

                        | CancelRequest, Some (Running (_, cts)) ->
                            let requestCount = requestCounts.TryGetValue key |> snd

                            if requestCount > 1 then
                                requestCounts.[key] <- requestCount - 1

                            else
                                cts.Cancel()
                                cache.Remove(key)
                                requestCounts.Remove key |> ignore
                                log (Canceled, key)

                        | CancelRequest, None
                        | CancelRequest, Some (Completed _) -> ()

                        | JobCompleted result, Some (Running _)
                        // Job could be evicted from cache while it's running
                        | JobCompleted result, None ->
                            cache.Set(key, (Completed(node.Return result)))
                            requestCounts.Remove key |> ignore
                            log (Finished, key)

                        | JobCompleted _result, Some (_job) -> failwith "If this happens there's a bug"
                    with
                    | :? OperationCanceledException as e ->
                        System.Diagnostics.Trace.TraceError($"AsyncMemoize OperationCanceledException: {e.Message}")
                    | ex -> System.Diagnostics.Trace.TraceError($"AsyncMemoize Exception: %A{ex}")
            })

    member _.Get(key, computation) =
        node {
            let! ct = NodeCode.CancellationToken

            let! job =
                agent.PostAndAsyncReply(fun rc -> key, (GetOrCompute(computation, ct)), rc)
                |> NodeCode.AwaitAsync

            return! job
        }
