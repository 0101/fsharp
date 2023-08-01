module CompilerService.AsyncMemoize

open System
open System.Threading
open Xunit
open FSharp.Test
open FSharp.Compiler.BuildGraph
open Internal.Utilities.Collections
open System.Threading.Tasks
open System.Diagnostics
open System.Collections.Concurrent
open Microsoft.VisualStudio.FSharp.Editor.CancellableTasks


[<Fact>]
let ``Stack trace`` () = 

    let eventLog = ResizeArray()

    let memoize = AsyncMemoize(logEvent=(fun _ -> eventLog.Add))

    let computation key = cancellableTask {
       // do! Async.Sleep 1 |> NodeCode.AwaitAsync

        let! result = memoize.Get(key * 2, cancellableTask {
            //do! Async.Sleep 1 |> NodeCode.AwaitAsync
            return key * 5
        })

        return result * 2
    }

    //let _r2 = computation 10

    let result = memoize.Get(1, computation 1) CancellationToken.None
    
    Assert.Equal(10, result.Result)


[<Fact>]
let ``Stack trace node`` () = task {

    let computation key = node {
        //do! Async.Sleep 1 |> NodeCode.AwaitAsync
        
        let! result = node {
            //do! Async.Sleep 1 |> NodeCode.AwaitAsync
            return key * 5
        }
        
        return result * 2
    }

    let! rsult = computation 1 |> Async.AwaitNodeCode

    Assert.Equal(10, rsult)
}

[<Fact>]
let ``Stack trace task`` () = task {

    let computation key = task {
        //do! Task.Delay 1

        let! result = task {
            //do! Task.Delay 1
            return key * 5
        }

        return result * 2
    }

    let! rsult = computation 1

    Assert.Equal(10, rsult)
}

[<Fact>]
let ``Basics``() =

    let computation key = cancellableTask {
        do! Task.Delay 1
        return key * 2
    }

    let eventLog = ResizeArray()

    let memoize = AsyncMemoize(logEvent=(fun _ -> eventLog.Add))

    let ct = CancellationToken.None

    let task =
        seq {
            memoize.Get(5, computation 5) ct
            memoize.Get(5, computation 5) ct
            memoize.Get(2, computation 2) ct
            memoize.Get(5, computation 5) ct
            memoize.Get(3, computation 3) ct
            memoize.Get(2, computation 2) ct
        }
        |> Task.WhenAll

    let result = task.Result
    let expected = [| 10; 10; 4; 10; 6; 4|]

    Assert.Equal<int array>(expected, result)

    let groups = eventLog |> Seq.groupBy snd |> Seq.toList
    Assert.Equal(3, groups.Length)
    for key, events in groups do
        Assert.Equal<(JobEvent * int) array>([| Started, key; Finished, key |], events |> Seq.toArray)

[<Fact>]
let ``We can cancel a job`` () =
    task {

        let jobStarted = new ManualResetEvent(false)

        let computation key = cancellableTask {
            jobStarted.Set() |> ignore
            do! Task.Delay 1000 
            failwith "Should be canceled before it gets here"
            return key * 2
        }

        let eventLog = ResizeArray()
        let memoize = AsyncMemoize(logEvent=(fun _ -> eventLog.Add))

        use cts1 = new CancellationTokenSource()
        use cts2 = new CancellationTokenSource()
        use cts3 = new CancellationTokenSource()

        let key = 1


        let _task1 = memoize.Get(key, computation key) cts1.Token
        let _task2 = memoize.Get(key, computation key) cts2.Token
        let _task3 = memoize.Get(key, computation key) cts3.Token

        jobStarted.WaitOne() |> ignore
        do! memoize.Sync()

        Assert.Equal<(JobEvent * int) array>([| Started, key |], eventLog |> Seq.toArray )

        cts1.Cancel()
        cts2.Cancel()

        do! memoize.Sync()

        Assert.Equal<(JobEvent * int) array>([| Started, key; Started, key |], eventLog |> Seq.toArray )

        cts3.Cancel()

        do! memoize.Sync() 

        Assert.Equal<(JobEvent * int) array>([| Started, key; Started, key; Canceled, key |], eventLog |> Seq.toArray )
    }

[<Fact>]
let ``Job keeps running even if first requestor cancels`` () =
    task {
        let jobStarted = new ManualResetEvent(false)

        let computation key = cancellableTask {
            jobStarted.Set() |> ignore

            for _ in 1 .. 10 do
                do! Task.Delay 1000

            return key * 2
        }

        let eventLog = ConcurrentBag()
        let memoize = AsyncMemoize(logEvent=(fun _ -> eventLog.Add))

        use cts1 = new CancellationTokenSource()
        use cts2 = new CancellationTokenSource()
        use cts3 = new CancellationTokenSource()

        let key = 1

        let _task1 = memoize.Get(key, computation key) cts1.Token
        let _task2 = memoize.Get(key, computation key) cts2.Token
        let _task3 = memoize.Get(key, computation key) cts3.Token

        //do! memoize.Sync()
        jobStarted.WaitOne() |> ignore

        cts1.Cancel()
        //do! memoize.Sync()
        
        do! Task.Delay 1000
        Assert.Equal(TaskStatus.Canceled, _task1.Status)
        

        cts3.Cancel()
        //do! memoize.Sync()
        
        let! result = _task2
        Assert.Equal(2, result)

        //do! memoize.Sync()

        Assert.Equal<(JobEvent * int) array>([| Started, key; Started, key; Finished, key |], eventLog |> Seq.toArray )

    }

type ExpectedException() =
    inherit Exception()

//[<Fact>]
let ``Stress test`` () =

    let seed = System.Random().Next()

    let rng = System.Random seed
    let threads = 30
    let iterations = 100
    let maxDuration = 100
    let minTimeout = 0
    let maxTimeout = 1000
    let exceptionProbability = 0.01
    let gcProbability = 0.1
    let stepMs = 10
    let keyCount = 20
    let keys = [| 1 .. keyCount |]

    let intenseComputation durationMs result =
        cancellableTask {
            if rng.NextDouble() < exceptionProbability then
                raise (ExpectedException())
            let s = Stopwatch.StartNew()
            let mutable number = 0
            while (int s.ElapsedMilliseconds) < durationMs do
                number <- number + 1 % 12345
            return [result]
        }
        

    let rec sleepyComputation durationMs result =
        cancellableTask {
            if rng.NextDouble() < (exceptionProbability / (float durationMs / float stepMs)) then
                raise (ExpectedException())
            if durationMs > 0 then
                do! Task.Delay (min stepMs durationMs)
                return! sleepyComputation (durationMs - stepMs) result
            else
                return [result]
        }

    let rec mixedComputation durationMs result =
        cancellableTask {
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

    let _cacheEvents = ConcurrentBag()

    //let cache = AsyncMemoize(fun _ x -> cacheEvents.Enqueue x)
    //let cache = AsyncMemoize(keepStrongly=5, keepWeakly=10, logEvent=(fun _ x -> cacheEvents.Add (DateTime.Now.Ticks, x)))
    let cache = AsyncMemoize(keepStrongly=5, keepWeakly=10)

    let mutable started = 0
    let mutable canceled = 0
    let mutable timeout = 0
    let mutable failed = 0
    let mutable completed = 0
    task {
        let! _x =
            seq {
                for _ in 1..threads do
                    let rec loop iteration =
                        task {
                            if gcProbability > rng.NextDouble() then
                                GC.Collect(2, GCCollectionMode.Forced, false)

                            let computation = computations[rng.Next computations.Length]
                            let durationMs = rng.Next maxDuration
                            let timeoutMs = rng.Next(minTimeout, maxTimeout)
                            let key = keys[rng.Next keys.Length]
                            let result = key * 2
                            let job = cache.Get(key, computation durationMs result)
                            let cts = new CancellationTokenSource()
                            let runningJob = job cts.Token
                            cts.CancelAfter timeoutMs
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
                                    ex.Flatten().InnerExceptions |> Seq.exists (fun e -> e :? ExpectedException) ->
                                    Interlocked.Increment &failed |> ignore
                                | e ->
                                    failwith $"Seed {seed} failed on iteration {iteration}: %A{e}"
                            if iteration < iterations then
                                return! loop (iteration + 1)
                            return ()
                        }
                    loop 1
            }
            |> Task.WhenAll
          
        //let task =
        //    async {

        
        //        do! Async.Sleep 500


    
        //        //let events =
        //        //    cacheEvents
        //        //    |> Seq.countBy id
        //        //    |> Seq.toArray

        //        let events = cacheEvents |> Seq.sortBy fst |> Seq.toArray

        //        ignore events
        //    }
        //    |> Async.StartAsTask
        //task.Wait()

        //ignore cacheEvents

        Assert.Equal (threads * iterations, started)
        //Assert.Equal<int * int * int * int * int>((0,0,0,0,0),(started, completed, canceled, failed, timeout))
        Assert.Equal (started, completed + canceled + failed + timeout)

        Assert.True ((float completed) > ((float started) * 0.1), "Less than 10% completed jobs")
    }
