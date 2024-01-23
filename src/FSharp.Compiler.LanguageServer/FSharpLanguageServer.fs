namespace FSharp.Compiler.LanguageServer

open System
open System.Threading.Tasks
open System.Threading
open Microsoft.CommonLanguageServerProtocol.Framework.Handlers
open Microsoft.CommonLanguageServerProtocol.Framework
open Microsoft.Extensions.DependencyInjection
open Microsoft.VisualStudio.LanguageServer.Protocol

open StreamJsonRpc


type FSharpRequestContext(lspServices: ILspServices, logger: ILspLogger) =
    member val LspServices = lspServices with get, set
    member val Logger = logger with get, set

type FShapRequestContextFactory(lspServices: ILspServices) =
    inherit AbstractRequestContextFactory<FSharpRequestContext>()

    override _.CreateRequestContextAsync<'TRequestParam>(queueItem: IQueueItem<FSharpRequestContext>, methodHandler: IMethodHandler, requestParam: 'TRequestParam, cancellationToken: CancellationToken) =
        let logger = lspServices.GetRequiredService<ILspLogger>()
        let requestContext = FSharpRequestContext(lspServices, logger)
        Task.FromResult(requestContext)


type CapabilitiesManager() =

    let mutable initializeParams = None

    interface IInitializeManager<InitializeParams, InitializeResult> with
        member this.SetInitializeParams(request) =
            initializeParams <- Some request

        member this.GetInitializeResult() =
            //let serverCapabilities =
            //    ServerCapabilities(SemanticTokensOptions(Range = true))

            InitializeResult(Capabilities = null)

        member this.GetInitializeParams() =
            match initializeParams with
            | Some params' -> params'
            | None -> failwith "InitializeParams is null"


type LspServiceLifeCycleManager() =

    interface ILifeCycleManager with
        member this.ShutdownAsync(message:string) = task {
            try
                printfn "Shutting down"
            with
            | :? ObjectDisposedException
            | :? ConnectionLostException -> ()
        }
        member this.ExitAsync() = Task.CompletedTask


type FSharpLspServices(serviceCollection: IServiceCollection) as this =

    do
        serviceCollection.AddSingleton<ILspServices>(this) |> ignore

    let serviceProvider = serviceCollection.BuildServiceProvider()

    interface ILspServices with
        member this.GetRequiredService<'T>(): 'T =
            serviceProvider.GetRequiredService<'T >()

        member this.TryGetService(t) = serviceProvider.GetService(t);

        member this.GetRequiredServices() = serviceProvider.GetServices()

        member this.GetRegisteredServices() = failwith "Not implemented"

        member this.SupportsGetRegisteredServices() = false

        member this.Dispose() = ()


type FSharpLanguageServer(jsonRpc: JsonRpc, logger: ILspLogger, addExtraHandlers: Action<IServiceCollection> option) =
    inherit AbstractLanguageServer<FSharpRequestContext>(jsonRpc, logger)

    let mutable _addExtraHandlers = addExtraHandlers

    do
        // This spins up the queue and ensure the LSP is ready to start receiving requests
        base.Initialize()

    member private this.GetBaseHandlerProvider() =
        base.HandlerProvider

    override this.ConstructLspServices() =
        let serviceCollection = new ServiceCollection()

        let _ =
            serviceCollection
                .AddSingleton<IMethodHandler, InitializeHandler<InitializeParams, InitializeResult, FSharpRequestContext>>()
                .AddSingleton<IMethodHandler, InitializedHandler<InitializedParams, FSharpRequestContext>>()
                .AddSingleton<ILspLogger>(logger)
                .AddSingleton<AbstractRequestContextFactory<FSharpRequestContext>, FShapRequestContextFactory>()
                .AddSingleton<AbstractHandlerProvider>(fun _ -> this.GetBaseHandlerProvider())
                .AddSingleton<IInitializeManager<InitializeParams, InitializeResult>, CapabilitiesManager>()
                .AddSingleton(this)
                .AddSingleton<ILifeCycleManager>(new LspServiceLifeCycleManager())

        match _addExtraHandlers with
        | Some handler -> handler.Invoke(serviceCollection)
        | None -> ()

        let lspServices = new FSharpLspServices(serviceCollection)

        lspServices :> ILspServices