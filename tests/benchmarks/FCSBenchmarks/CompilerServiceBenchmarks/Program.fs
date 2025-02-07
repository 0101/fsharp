open BenchmarkDotNet.Running
open FSharp.Compiler.Benchmarks
open BenchmarkDotNet.Configs

[<EntryPoint>]
let main args =
#if Debug
    let cfg = DebugInProcessConfig()
#else
    let cfg = ManualConfig.Create(DefaultConfig.Instance).WithOptions(ConfigOptions.DisableOptimizationsValidator)
#endif
    BenchmarkSwitcher.FromAssembly(typeof<DecentlySizedStandAloneFileBenchmark>.Assembly).Run(args,cfg) |> ignore
    0
