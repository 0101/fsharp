// F# does not have an exact equivalent to C#'s namespaces, but you can use modules for a similar effect
module Microsoft.CommonLanguageServerProtocol.Framework.Example

open System
open System.Threading.Tasks
open System.Threading
open Microsoft.CommonLanguageServerProtocol.Framework.Handlers
open Microsoft.Extensions.DependencyInjection
open Roslyn.LanguageServer.Protocol
open StreamJsonRpc
open System.Runtime.Serialization
open Newtonsoft.Json

open Ionide.LanguageServerProtocol.Types

type ExampleRequestContext(lspServices: ILspServices, logger: ILspLogger) =
    member val LspServices = lspServices with get, set
    member val Logger = logger with get, set

type ExampleRequestContextFactory(lspServices: ILspServices) =
    interface IRequestContextFactory<ExampleRequestContext> with
        member this.CreateRequestContextAsync<'TRequestParam>(queueItem: IQueueItem<ExampleRequestContext>, param: 'TRequestParam, cancellationToken: CancellationToken) =
            let logger = lspServices.GetRequiredService<ILspLogger>()
            let requestContext = ExampleRequestContext(lspServices, logger)
            Task.FromResult(requestContext)



//[<DataContract>]
//type InitializeParams() =
//    [<DataMember(Name = "processId")>]
//    [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
//    member val ProcessId : int = 0 with get, set
//    [<DataMember(Name = "locale")>]
//    [<JsonProperty(NullValueHandling = NullValueHandling.Ignore)>]
//    member val Locale : string = null with get, set
//    [<DataMember(Name = "rootPath")>]
//    [<JsonProperty(NullValueHandling = NullValueHandling.Ignore)>]
//    [<Obsolete("Deprecated in favour of RootUri")>]
//    member val RootPath : string = null with get, set
//    [<DataMember(Name = "rootUri")>]
//    //[<JsonConverter(typeof<DocumentUriConverter>)>]
//    [<JsonProperty(NullValueHandling = NullValueHandling.Include)>]
//    member val RootUri : Uri = null with get, set
//    [<DataMember(Name = "initializationOptions")>]
//    [<JsonProperty(NullValueHandling = NullValueHandling.Ignore)>]
//    member val InitializationOptions : obj = null with get, set
//    [<DataMember(Name = "capabilities")>]
//    member val Capabilities : obj = null with get, set
//    [<DataMember(Name = "trace")>]
//    //[<DefaultValue(typeof<TraceSetting>, "off")>]
//    [<JsonProperty(DefaultValueHandling = DefaultValueHandling.Ignore)>]
//    member val Trace : string = "off" with get, set


[<DataContract>]
type InitializeResult() =
    [<DataMember(Name = "capabilities")>]
    [<JsonProperty(NullValueHandling = NullValueHandling.Ignore)>]
    member val Capabilities : obj = null with get, set
  
  
//type InitializedParams() = class end



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


//type ExampleRequestContext = unit

type ILspService = interface end


type LspServiceLifeCycleManager() =
    interface ILspService

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


type ExampleLanguageServer(jsonRpc: JsonRpc, logger: ILspLogger, addExtraHandlers: Action<IServiceCollection> option) =
    inherit AbstractLanguageServer<ExampleRequestContext>(jsonRpc, logger)

    let mutable _addExtraHandlers = addExtraHandlers

    do
        // This spins up the queue and ensure the LSP is ready to start receiving requests
        base.Initialize()

    member private this.GetBaseHandlerProvider() =
        base.GetHandlerProvider()

    override this.ConstructLspServices() =
        let serviceCollection = new ServiceCollection()

        //let handlerProvider = base.GetHandlerProvider()

        let _ = 
            serviceCollection
                .AddSingleton<IMethodHandler, InitializeHandler<InitializedParams, InitializeResult, ExampleRequestContext>>()
                .AddSingleton<IMethodHandler, InitializedHandler<InitializedParams, ExampleRequestContext>>()
                .AddSingleton<ILspLogger>(logger)
                .AddSingleton<IRequestContextFactory<ExampleRequestContext>, ExampleRequestContextFactory>()
                .AddSingleton<IHandlerProvider>(fun s -> this.GetBaseHandlerProvider())
                .AddSingleton<IInitializeManager<InitializeParams, InitializeResult>, CapabilitiesManager>()
                .AddSingleton<AbstractLanguageServer<ExampleRequestContext>>(this)
                .AddSingleton<ILifeCycleManager>(new LspServiceLifeCycleManager())

        match _addExtraHandlers with
        | Some handler -> handler.Invoke(serviceCollection)
        | None -> ()

        let lspServices = new FSharpLspServices(serviceCollection)

        lspServices :> ILspServices


[<EntryPoint>]
let main argv =

    let jsonRpc = new JsonRpc(Console.OpenStandardOutput(), Console.OpenStandardInput())
    
    let logger = 
        { new ILspLogger with
            member this.LogEndContext(message: string, ``params``: obj array): unit = 
                printfn "%s" message
            member this.LogError(message: string, ``params``: obj array): unit = 
                printfn "%s" message
            member this.LogException(``exception``: exn, message: string, ``params``: obj array): unit = 
                printfn "%s" message
            member this.LogInformation(message: string, ``params``: obj array): unit = 
                printfn "%s" message
            member this.LogStartContext(message: string, ``params``: obj array): unit = 
                printfn "%s" message
            member this.LogWarning(message: string, ``params``: obj array): unit = 
                printfn "%s" message

                            }
    
    let s = new ExampleLanguageServer(jsonRpc, logger, None)
    
    jsonRpc.StartListening()

    async {
        while true do
            do! Async.Sleep 1000         

    
    }
    |> Async.RunSynchronously

    0