namespace Internal.Utilities.Collections

open System
open System.Collections.Generic
open System.Threading
open System.Diagnostics.Tracing

open FSharp.Compiler.BuildGraph

[<EventSource(Name = "FSharpCompilerService.AsyncCache")>]
type AsyncMemoizeEventSource(name) =

    inherit EventSource()

    let mutable _requestCounter = None

    member _.ReportSize value =
        _requestCounter
        |> Option.iter (fun (ec: EventCounter) -> ec.WriteMetric(float32 value))

    override this.OnEventCommand(args: EventCommandEventArgs) =
        //if args.Command = EventCommand.Enable && _requestCounter = None then
        ignore args

        if _requestCounter = None then
            let ec = EventCounter($"Cache-size|%s{name}", this)
            _requestCounter <- Some ec

    override _.Dispose(disposing) =
        _requestCounter <- None
        base.Dispose(disposing)

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

type internal AsyncMemoize<'TKey, 'TValue when 'TKey: equality>(?logEvent: (string -> JobEventType * 'TKey -> unit), ?name: string) =

    let name = defaultArg name "N/A"
    let tok = obj ()

    //let es = new AsyncMemoizeEventSource(name)

    let cache =
        MruCache<_, 'TKey, Job<'TValue>>(
            keepStrongly = 200,
            keepMax = 400,
            areSame = (fun (x, y) -> x = y),
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

                        //es.ReportSize (cache.GetSize())

                        let! key, action, replyChannel = inbox.Receive()

                        match action, cache.TryGet(tok, key) with
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

                            cache.Set(tok, key, (Running(job, cts)))

                            incrRequestCount key

                            ct.Register(fun _ -> post key CancelRequest) |> ignore

                            replyChannel.Reply job

                        | CancelRequest, Some (Running (_, cts)) ->
                            let requestCount = requestCounts.TryGetValue key |> snd

                            if requestCount > 1 then
                                requestCounts.[key] <- requestCount - 1

                            else
                                cts.Cancel()
                                cache.RemoveAnySimilar(tok, key)
                                requestCounts.Remove key |> ignore
                                log (Canceled, key)

                        | CancelRequest, None
                        | CancelRequest, Some (Completed _) -> ()

                        | JobCompleted result, Some (Running _)
                        // Job could be evicted from cache while it's running
                        | JobCompleted result, None ->
                            cache.Set(tok, key, (Completed(node.Return result)))
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
