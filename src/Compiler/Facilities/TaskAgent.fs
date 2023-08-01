namespace Internal.Utilities.TaskAgent

open System.Threading
open System.Threading.Tasks

open System.Collections.Concurrent


type AgentMessage<'Message, 'MessageNoReply, 'Reply> =
    | ExpectsReply of 'Message * TaskCompletionSource<'Reply>
    | DoNotReply of 'MessageNoReply


type TaskInbox<'Msg, 'MsgNoReply, 'Reply>() =

    let queue = ConcurrentQueue<AgentMessage<'Msg, 'MsgNoReply, 'Reply>>()

    let messageNotifications = new SemaphoreSlim(0) // todo: Dispose?

    member _.PostAndAwaitReply(msg) =
        let replySource = TaskCompletionSource<'Reply>()

        queue.Enqueue (ExpectsReply (msg, replySource))

        messageNotifications.Release() |> ignore

        replySource.Task

    member _.Post(msg) =
        queue.Enqueue (DoNotReply msg)
        messageNotifications.Release() |> ignore

    member _.Receive() = task {
        do! messageNotifications.WaitAsync()

        return
            match queue.TryDequeue() with
            | true, msg -> msg
            | false, _ -> failwith "Message notifications broken"
    }


type TaskAgent<'Msg, 'MsgNoReply, 'Reply>(
    processMessage: ('MsgNoReply -> unit) -> 'Msg -> 'Reply,
    processMessageNoReply: ('MsgNoReply -> unit) -> 'MsgNoReply -> unit) =
    let inbox = TaskInbox<'Msg, 'MsgNoReply, 'Reply>()

    let exceptionEvent = new Event<_>()

    let _loop = backgroundTask {
        while true do
            match! inbox.Receive() with
            | ExpectsReply (msg, replySource) ->
                try
                    let reply = processMessage inbox.Post msg
                    replySource.SetResult reply
                with ex ->
                    replySource.SetException ex

            | DoNotReply msg ->
                try
                    do processMessageNoReply inbox.Post msg
                with ex ->
                    exceptionEvent.Trigger (msg, ex)
    }

    member _.NoReplyExceptions = exceptionEvent.Publish

    member _.Status = _loop.Status

    member _.PostAndAwaitReply(msg) = inbox.PostAndAwaitReply(msg)

    member _.Post(msg) = inbox.Post(msg)

