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

/// An incoming LSP message. Requests carry an ``id`` field; notifications do not.
type LSPMessage =
    | RequestMsg of headers: Dictionary<string, string> * content: JObject
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

/// Send an LSP error response (has an ``id`` and an ``error`` field).
let sendErrorResponse (id: int) (code: int) (message: string) =
    sendRaw (JObject.FromObject(ErrorResponse(id, ErrorObject(code, message))))

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

let private tryParseHeaderLine (line: string) : (string * string) option =
    let parts = line.Split([| ": " |], 2, StringSplitOptions.None)
    if parts.Length = 2 then Some(parts.[0], parts.[1]) else None

let private readHeaders () : Dictionary<string, string> option =
    let headers = Dictionary<string, string>()

    let rec readFirstHeader () =
        let line = Console.ReadLine()
        if isNull line then None
        elif line.Length = 0 then readFirstHeader ()
        else Some line

    match readFirstHeader () with
    | None -> None
    | Some first ->
        let mutable valid = true
        match tryParseHeaderLine first with
        | Some(k, v) -> headers.[k] <- v
        | None -> valid <- false

        while valid do
            let line = Console.ReadLine()
            if isNull line || line.Length = 0 then
                valid <- false
            else
                match tryParseHeaderLine line with
                | Some(k, v) -> headers.[k] <- v
                | None -> valid <- false

        if headers.Count = 0 then None else Some headers

let private readBody (contentLength: int) : string option =
    let sb = StringBuilder(contentLength)
    let mutable ok = true
    let mutable i = 0
    while ok && i < contentLength do
        let ch = Console.Read()
        if ch < 0 then
            ok <- false
        else
            sb.Append(char ch) |> ignore
            i <- i + 1

    if ok then Some(sb.ToString()) else None

/// Block until a complete LSP message arrives on stdin, then return it.
/// Returns ``None`` if stream ended or message framing/content is invalid.
let waitMessage () : LSPMessage option =
    match readHeaders () with
    | None -> None
    | Some headers ->
        let contentLength =
            match headers.TryGetValue "Content-Length" with
            | true, value ->
                match Int32.TryParse value with
                | true, n when n >= 0 -> Some n
                | _ -> None
            | _ -> None

        match contentLength with
        | None -> None
        | Some len ->
            match readBody len with
            | None -> None
            | Some body ->
                try
                    let json = JObject.Parse(body)
                    if json.ContainsKey "id" then Some(RequestMsg(headers, json))
                    else Some(NotificationMsg(headers, json))
                with _ ->
                    None

// ---------------------------------------------------------------------------
// Accessors that work on the raw JObject (used in Program.fs)
// ---------------------------------------------------------------------------

let messageContent = function
    | RequestMsg(_, c) -> c
    | NotificationMsg(_, c) -> c

let tryRequestId (content: JObject) : int option =
    match content.TryGetValue "id" with
    | true, token ->
        match token.Type with
        | JTokenType.Integer -> token.Value<int>() |> Some
        | _ ->
            match Int32.TryParse(token.ToString()) with
            | true, value -> Some value
            | _ -> None
    | _ -> None

let messageMethod (content: JObject) : string option =
    match content.TryGetValue "method" with
    | true, token -> Some(token.ToString())
    | _ -> None

let messageParams (content: JObject) : JToken option =
    match content.TryGetValue "params" with
    | true, token -> Some token
    | _ -> None
