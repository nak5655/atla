/// Reading and writing LSP messages over stdin/stdout.
module Atla.LanguageServer.LSPMessage

open System
open System.Collections.Generic
open System.Text
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Atla.LanguageServer.LSPTypes

// ---------------------------------------------------------------------------
// Message discriminated union
// ---------------------------------------------------------------------------

/// An incoming LSP message.  Requests carry an ``id`` field; notifications do not.
type LSPMessage =
    | RequestMsg      of headers: Dictionary<string, string> * content: JObject
    | NotificationMsg of headers: Dictionary<string, string> * content: JObject

// ---------------------------------------------------------------------------
// Sending helpers
// ---------------------------------------------------------------------------

/// Low-level writer: adds the ``jsonrpc`` field, computes Content-Length, and
/// writes headers + body to stdout.
let private sendRaw (content: JObject) =
    content.["jsonrpc"] <- JValue "2.0"
    let body = content.ToString(Formatting.None)
    let bodyBytes = Encoding.UTF8.GetBytes body
    Console.WriteLine(sprintf "Content-Length: %d" bodyBytes.Length)
    Console.WriteLine()
    Console.Write body
    Console.Out.Flush()

/// Send an LSP response (has an ``id`` and a ``result`` field).
let sendResponse (id: int) (result: obj) =
    sendRaw (JObject.FromObject(Response(id, result)))

/// Send an LSP notification (has a ``method`` and a ``params`` field).
let sendNotification (method: string) (``params``: obj) =
    sendRaw (JObject.FromObject(LSPNotification(method, ``params``)))

/// Send a ``window/logMessage`` notification.
let windowLogMessage (message: string) (msgType: MessageType) =
    sendNotification "window/logMessage" (LogMessageParams(msgType, message))

/// Send a ``textDocument/publishDiagnostics`` notification.
let publishDiagnostics (uri: string) (diagnostics: Diagnostic list) =
    sendNotification "textDocument/publishDiagnostics" (PublishDiagnosticsParams(uri, diagnostics))

// ---------------------------------------------------------------------------
// Receiving
// ---------------------------------------------------------------------------

/// Block until a complete LSP message arrives on stdin, then return it.
let waitMessage () : LSPMessage =
    let headers = Dictionary<string, string>()

    // Read header lines until we hit the blank separator line.
    let mutable readingHeaders = true
    while readingHeaders do
        let line = Console.ReadLine()
        if isNull line || line.Length = 0 then
            readingHeaders <- false
        else
            let parts = line.Split([| ": " |], 2, StringSplitOptions.None)
            if parts.Length = 2 then
                headers.[parts.[0]] <- parts.[1]
            else
                readingHeaders <- false

    let contentLength = int headers.["Content-Length"]
    let sb = StringBuilder(contentLength)
    for _ in 1 .. contentLength do
        sb.Append(Console.Read() |> char) |> ignore

    let json = JObject.Parse(sb.ToString())

    if json.ContainsKey "id" then
        RequestMsg(headers, json)
    else
        NotificationMsg(headers, json)

// ---------------------------------------------------------------------------
// Accessors that work on the raw JObject (used in Program.fs)
// ---------------------------------------------------------------------------

let messageContent = function
    | RequestMsg(_, c)      -> c
    | NotificationMsg(_, c) -> c

let requestId (content: JObject) =
    int (content.["id"].ToString())

let messageMethod (content: JObject) =
    content.["method"].ToString()

let messageParams (content: JObject) : JToken =
    content.["params"]
