module LanguageServerTests

open System
open Xunit

open FSharp.Compiler.LanguageServer
open StreamJsonRpc
open System.IO
open System.Diagnostics


[<Fact>]
let ``The server can process the initialization message`` () =

     // Create a StringWriter to capture the output
    let rpcTrace = new StringWriter()

    try

    let output = new System.IO.MemoryStream()
    let input = new System.IO.MemoryStream()
    let writer = new System.IO.StreamWriter(output, leaveOpen=true)
    let reader = new System.IO.StreamReader(input)

    let jsonRpc = new JsonRpc(output, input)

    // Create a new TraceListener with the StringWriter
    let listener = new TextWriterTraceListener(rpcTrace)

    // Add the listener to the JsonRpc TraceSource
    jsonRpc.TraceSource.Listeners.Add(listener) |> ignore

    // Set the TraceLevel to Information to get all informational, warning and error messages
    jsonRpc.TraceSource.Switch.Level <- SourceLevels.Information

    //jsonRpc.inv

    // Now all JsonRpc debug information will be written to the StringWriter

    let log = ResizeArray()

    let _s = new FSharpLanguageServer(jsonRpc, (LspLogger log.Add) , None)

    jsonRpc.StartListening()

    let initializeMessage = """{
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "processId": 123,
            "rootUri": "file:///home/user/fsharp",
            "capabilities": {},
            "trace": "off"
        }
    }"""

    let header = $"Content-Length: {initializeMessage.Length}\r\n\r\n"

    writer.Write(header)
    writer.Write(initializeMessage)
    //writer.Write("\r\n")

    let response = reader.ReadLine()

    Assert.Equal("Content-Length: 101\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"capabilities\":null}}", response)

    finally

    // You can get the output as a string like this:
        let _output = rpcTrace.ToString()
        ()