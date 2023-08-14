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

    let memoize = AsyncMemoize<int, int, int>(logEvent=(fun _ -> eventLog.Add))

    let computation key = cancellableTask {
       // do! Async.Sleep 1 |> NodeCode.AwaitAsync

        let! result = memoize.Get'(key * 2, cancellableTask {
            //do! Async.Sleep 1 |> NodeCode.AwaitAsync
            return key * 5
        })

        return result * 2
    }

    //let _r2 = computation 10

    let result = memoize.Get'(1, computation 1) CancellationToken.None

    Assert.Equal(10, result.Result)


[<Fact>]
let ``Basics``() =

    let computation key = cancellableTask {
        do! Task.Delay 1
        return key * 2
    }

    let eventLog = ResizeArray()

    let memoize = AsyncMemoize<int, int, int>(logEvent=(fun _ (e, (_label, k, _version)) -> eventLog.Add (e, k)))

    let ct = CancellationToken.None

    let task =
        seq {
            memoize.Get'(5, computation 5) ct
            memoize.Get'(5, computation 5) ct
            memoize.Get'(2, computation 2) ct
            memoize.Get'(5, computation 5) ct
            memoize.Get'(3, computation 3) ct
            memoize.Get'(2, computation 2) ct
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
        let memoize = AsyncMemoize<int, int, int>(logEvent=(fun _ (e, (_, k, _version)) -> eventLog.Add (e, k)))

        use cts1 = new CancellationTokenSource()
        use cts2 = new CancellationTokenSource()
        use cts3 = new CancellationTokenSource()

        let key = 1


        let _task1 = memoize.Get'(key, computation key) cts1.Token
        let _task2 = memoize.Get'(key, computation key) cts2.Token
        let _task3 = memoize.Get'(key, computation key) cts3.Token

        jobStarted.WaitOne() |> ignore
        do! memoize.Sync()

        jobStarted.Reset() |> ignore

        Assert.Equal<(JobEvent * int) array>([| Started, key |], eventLog |> Seq.toArray )

        cts1.Cancel()
        cts2.Cancel()

        jobStarted.WaitOne() |> ignore

        do! memoize.Sync()

        Assert.Equal<(JobEvent * int) array>([| Started, key; Started, key |], eventLog |> Seq.toArray )

        cts3.Cancel()

        do! Task.Delay 100
        do! memoize.Sync() 

        Assert.Equal<(JobEvent * int) array>([| Started, key; Started, key; Canceled, key |], eventLog |> Seq.toArray )
    }

[<Fact>]
let ``Job is restarted if first requestor cancels`` () =
    task {
        let jobStarted = new ManualResetEvent(false)

        let computation key = cancellableTask {
            jobStarted.Set() |> ignore

            for _ in 1 .. 5 do
                do! Task.Delay 100

            return key * 2
        }

        let eventLog = ConcurrentBag()
        let memoize = AsyncMemoize<int, int, int>(logEvent=(fun _ (e, (_, k, _version)) -> eventLog.Add (DateTime.Now.Ticks, (e, k))))

        use cts1 = new CancellationTokenSource()
        use cts2 = new CancellationTokenSource()
        use cts3 = new CancellationTokenSource()

        let key = 1

        let _task1 = memoize.Get'(key, computation key) cts1.Token
        let _task2 = memoize.Get'(key, computation key) cts2.Token
        let _task3 = memoize.Get'(key, computation key) cts3.Token


        jobStarted.WaitOne() |> ignore

        cts1.Cancel()

        do! Task.Delay 100
        cts3.Cancel()

        let! result = _task2
        Assert.Equal(2, result)

        Assert.Equal(TaskStatus.Canceled, _task1.Status)

        let orderedLog = eventLog |> Seq.sortBy fst |> Seq.map snd |> Seq.toList
        let expected = [ Started, key; Started, key; Finished, key ]

        Assert.Equal<_ list>(expected, orderedLog)
    }

[<Fact>]
let ``Job is restarted if first requestor cancels but keeps running if second requestor cancels`` () =
    task {
        let jobStarted = new ManualResetEvent(false)

        let computation key = cancellableTask {
            jobStarted.Set() |> ignore

            for _ in 1 .. 5 do
                do! Task.Delay 100

            return key * 2
        }

        let eventLog = ConcurrentBag()
        let memoize = AsyncMemoize<int, int, int>(logEvent=(fun _ (e, (_, k, _version)) -> eventLog.Add (DateTime.Now.Ticks, (e, k))))

        use cts1 = new CancellationTokenSource()
        use cts2 = new CancellationTokenSource()
        use cts3 = new CancellationTokenSource()

        let key = 1

        let _task1 = memoize.Get'(key, computation key) cts1.Token

        jobStarted.WaitOne() |> ignore
        let _task2 = memoize.Get'(key, computation key) cts2.Token
        let _task3 = memoize.Get'(key, computation key) cts3.Token

        cts1.Cancel()

        jobStarted.WaitOne() |> ignore

        cts2.Cancel()

        let! result = _task3
        Assert.Equal(2, result)

        Assert.Equal(TaskStatus.Canceled, _task1.Status)
        Assert.Equal(TaskStatus.Canceled, _task2.Status)

        let orderedLog = eventLog |> Seq.sortBy fst |> Seq.map snd |> Seq.toList
        let expected = [ Started, key; Started, key; Finished, key ]

        Assert.Equal<_ list>(expected, orderedLog)
    }


type ExpectedException() =
    inherit Exception()

[<Fact>]
let ``Stress test`` () =

    let seed = System.Random().Next()

    let rng = System.Random seed
    let threads = 30
    let iterations = 30
    let maxDuration = 100
    let minTimeout = 0
    let maxTimeout = 500
    let exceptionProbability = 0.01
    let gcProbability = 0.1
    let stepMs = 10
    let keyCount = rng.Next(5, 200)
    let keys = [| 1 .. keyCount |]

    let testTimeoutMs = threads * iterations * maxDuration / 2

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

    let cache = AsyncMemoize<int, int, int list>(keepStrongly=5, keepWeakly=10)

    let mutable started = 0
    let mutable canceled = 0
    let mutable timeout = 0
    let mutable failed = 0
    let mutable completed = 0

    let test =
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
                        let job = cache.Get'(key, computation durationMs result)
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
                            | :? OperationCanceledException as _e ->
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

    if not (test.Wait testTimeoutMs) then failwith "Test timed out - most likely deadlocked"

    Assert.Equal (threads * iterations, started)
    //Assert.Equal<int * int * int * int * int>((0,0,0,0,0),(started, completed, canceled, failed, timeout))
    Assert.Equal (started, completed + canceled + failed + timeout)

    Assert.True ((float completed) > ((float started) * 0.1), "Less than 10% completed jobs")
