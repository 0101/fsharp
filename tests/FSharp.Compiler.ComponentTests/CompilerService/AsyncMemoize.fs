module FSharp.Compiler.ComponentTests.CompilerService.AsyncMemoize

open System
open System.Threading
open Xunit
open FSharp.Test
open FSharp.Compiler.BuildGraph
open Internal.Utilities.Collections
open System.Threading.Tasks
open System.Diagnostics
open System.Collections.Concurrent

[<Fact>]
let ``Basics``() =

    let computation key = node {
        do! Async.Sleep 1 |> NodeCode.AwaitAsync
        return key * 2
    }

    let eventLog = ResizeArray()

    let memoize = AsyncMemoize(logEvent=(fun _ -> eventLog.Add))

    let task =
        NodeCode.Parallel(seq {
            memoize.Get(5, computation 5)
            memoize.Get(5, computation 5)
            memoize.Get(2, computation 2)
            memoize.Get(5, computation 5)
            memoize.Get(3, computation 3)
            memoize.Get(2, computation 2)
        }) |> NodeCode.StartAsTask_ForTesting

    let result = task.Result
    let expected = [| 10; 10; 4; 10; 6; 4|]

    Assert.Equal<int array>(expected, result)

    let groups = eventLog |> Seq.groupBy snd |> Seq.toList
    Assert.Equal(3, groups.Length)
    for key, events in groups do
        Assert.Equal<(JobEvent * int) array>([| Started, key; Finished, key |], events |> Seq.toArray)

[<Fact>]
let ``We can cancel a job`` () =

    let resetEvent = new ManualResetEvent(false)

    let computation key = node {
        resetEvent.Set() |> ignore
        do! Async.Sleep 1000 |> NodeCode.AwaitAsync
        failwith "Should be canceled before it gets here"
        return key * 2
    }

    let eventLog = ResizeArray()
    let memoize = AsyncMemoize(logEvent=(fun _ -> eventLog.Add))

    use cts1 = new CancellationTokenSource()
    use cts2 = new CancellationTokenSource()
    use cts3 = new CancellationTokenSource()

    let key = 1

    let _task1 = NodeCode.StartAsTask_ForTesting(memoize.Get(key, computation key), cts1.Token)
    let _task2 = NodeCode.StartAsTask_ForTesting(memoize.Get(key, computation key), cts2.Token)
    let _task3 = NodeCode.StartAsTask_ForTesting(memoize.Get(key, computation key), cts3.Token)

    resetEvent.WaitOne() |> ignore

    Assert.Equal<(JobEvent * int) array>([| Started, key |], eventLog |> Seq.toArray )

    cts1.Cancel()
    cts2.Cancel()

    Thread.Sleep 10

    Assert.Equal<(JobEvent * int) array>([| Started, key |], eventLog |> Seq.toArray )

    cts3.Cancel()

    Thread.Sleep 100

    Assert.Equal<(JobEvent * int) array>([| Started, key; Canceled, key |], eventLog |> Seq.toArray )

    try
        Task.WaitAll(_task1, _task2, _task3)
    with :? AggregateException as ex ->
        Assert.Equal(3, ex.InnerExceptions.Count)
        Assert.True(ex.InnerExceptions |> Seq.forall (fun e -> e :? TaskCanceledException))

[<Fact>]
let ``Job keeps running even if first requestor cancels`` () =
    let computation key = node {
        do! Async.Sleep 100 |> NodeCode.AwaitAsync
        return key * 2
    }

    let eventLog = ResizeArray()
    let memoize = AsyncMemoize(logEvent=(fun _ -> eventLog.Add))

    use cts1 = new CancellationTokenSource()
    use cts2 = new CancellationTokenSource()
    use cts3 = new CancellationTokenSource()

    let key = 1

    let _task1 = NodeCode.StartAsTask_ForTesting(memoize.Get(key, computation key), cts1.Token)
    let _task2 = NodeCode.StartAsTask_ForTesting(memoize.Get(key, computation key), cts2.Token)
    let _task3 = NodeCode.StartAsTask_ForTesting(memoize.Get(key, computation key), cts3.Token)

    Thread.Sleep 10

    cts1.Cancel()
    cts3.Cancel()

    let result = _task2.Result
    Assert.Equal(2, result)

    Thread.Sleep 1 // Wait for event log to be updated
    Assert.Equal<(JobEvent * int) array>([| Started, key; Finished, key |], eventLog |> Seq.toArray )


type ExpectedException() =
    inherit Exception()

[<Fact>]
let ``Stress test`` () =
    let seed = System.Random().Next()

    let rng = System.Random seed
    let threads = 100
    let iterations = 10
    let maxDuration = 100
    let minTimeout = 110
    let maxTimeout = 200
    let exceptionProbability = 0.01
    let gcProbability = 0.1
    let stepMs = 10
    let keyCount = 20
    let keys = [| 1 .. keyCount |]

    let intenseComputation durationMs result =
        async {
            if rng.NextDouble() < exceptionProbability then
                raise (ExpectedException())
            let s = Stopwatch.StartNew()
            let mutable number = 0
            while (int s.ElapsedMilliseconds) < durationMs do
                number <- number + 1 % 12345
            return [result]
        }
        |> NodeCode.AwaitAsync

    let rec sleepyComputation durationMs result =
        node {
            if rng.NextDouble() < (exceptionProbability / (float durationMs / float stepMs)) then
                raise (ExpectedException())
            if durationMs > 0 then
                do! Async.Sleep (min stepMs durationMs) |> NodeCode.AwaitAsync
                return! sleepyComputation (durationMs - stepMs) result
            else
                return [result]
        }

    let rec mixedComputation durationMs result =
        node {
            if durationMs > 0 then
                if rng.NextDouble() < 0.5 then
                    let! _ = intenseComputation (min stepMs durationMs) ()
                    ()
                else
                    let! _ = sleepyComputation (min stepMs durationMs) ()
                    ()
                return! mixedComputation (durationMs - stepMs) result
            else
                return [result]
        }

    let computations = [|
        intenseComputation
        sleepyComputation
        mixedComputation
    |]

    let cacheEvents = ConcurrentBag()

    //let cache = AsyncMemoize(fun _ x -> cacheEvents.Enqueue x)
    let cache = AsyncMemoize(keepStrongly=5, keepWeakly=10, logEvent=(fun _ x -> cacheEvents.Add (DateTime.Now.Ticks, x)))
    //let cache = AsyncMemoize(keepStrongly=5, keepWeakly=10)

    let mutable started = 0
    let mutable canceled = 0
    let mutable timeout = 0
    let mutable failed = 0
    let mutable completed = 0

    let task = 
        seq {
            for _ in 1..threads do
                let rec loop iteration =
                    async {
                        if gcProbability > rng.NextDouble() then
                            GC.Collect(2, GCCollectionMode.Forced, false)

                        let computation = computations[rng.Next computations.Length]
                        let durationMs = rng.Next maxDuration
                        let timeoutMs = rng.Next(minTimeout, maxTimeout)
                        let key = keys[rng.Next keys.Length]
                        let result = key * 2
                        let job = cache.Get(key, computation durationMs result) |> Async.AwaitNodeCode
                        let! runningJob = Async.StartChild(job)
                        ignore timeoutMs
                        Interlocked.Increment &started |> ignore
                        try
                            let! actual = runningJob
                            Assert.Equal(result, actual.Head)
                            Interlocked.Increment &completed |> ignore
                        with
                            | :? TaskCanceledException as _e -> 

                                Interlocked.Increment &canceled |> ignore
                            | :? TimeoutException -> Interlocked.Increment &timeout |> ignore
                            | :? ExpectedException -> Interlocked.Increment &failed |> ignore
                            | :? AggregateException as ex when
                                ex.InnerExceptions |> Seq.exists (fun e -> e :? ExpectedException) ->
                                Interlocked.Increment &failed |> ignore
                            | e ->
                                failwith $"Seed {seed} failed on iteration {iteration}: %A{e}"
                        if iteration < iterations then
                            return! loop (iteration + 1)
                        return ()
                    } 
                loop 1
        }
        |> Async.Parallel
        |> Async.StartAsTask

    task.Wait()
   
    let task = 
        async {

        
            do! Async.Sleep 500


    
            //let events =
            //    cacheEvents
            //    |> Seq.countBy id
            //    |> Seq.toArray

            let events = cacheEvents |> Seq.sortBy fst |> Seq.toArray

            ignore events
        }
        |> Async.StartAsTask
    task.Wait()

    ignore cacheEvents

    Assert.Equal (threads * iterations, started)
    Assert.Equal (started, completed + canceled + failed + timeout)
    
