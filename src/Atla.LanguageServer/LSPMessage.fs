/// Reading and writing LSP messages over stdin/stdout.
module Atla.LanguageServer.LSPMessage

open System
open System.Collections.Generic
open System.IO
open System.Text
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Atla.LanguageServer.LSPTypes

/// UTF-8 without BOM. The LSP wire protocol mandates UTF-8 and counts
/// ``Content-Length`` in BYTES (not characters); see the byte-accurate
/// reader/writer below.
let private utf8NoBom = UTF8Encoding(false)

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

/// Serialise access to stdout so that background threads (e.g. async compile
/// tasks that call publishDiagnostics) never interleave their output with the
/// main message-loop thread.
let private stdoutLock = obj()

/// Raw stdout stream. We write bytes directly (not via Console text APIs) so
/// the emitted byte count always matches the Content-Length header regardless
/// of the host's Console.OutputEncoding.
let private stdoutStream = Console.OpenStandardOutput()

/// Low-level writer: adds the ``jsonrpc`` field, computes the Content-Length in
/// UTF-8 BYTES, and writes header + body bytes to stdout.
let private sendRaw (content: JObject) =
    content.["jsonrpc"] <- JValue "2.0"
    let body = content.ToString(Formatting.None)
    let bodyBytes = utf8NoBom.GetBytes body
    let headerBytes = Encoding.ASCII.GetBytes(sprintf "Content-Length: %d\r\n\r\n" bodyBytes.Length)
    lock stdoutLock (fun () ->
        stdoutStream.Write(headerBytes, 0, headerBytes.Length)
        stdoutStream.Write(bodyBytes, 0, bodyBytes.Length)
        stdoutStream.Flush())

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

/// 1 行をバイト単位で読む（行末 ``\n`` まで、末尾 ``\r`` は除去）。ヘッダは ASCII。
/// ストリーム終端で 1 バイトも読めなければ ``None``。
let private readHeaderLine (stream: Stream) : string option =
    let buf = ResizeArray<byte>()
    let mutable b = stream.ReadByte()
    if b < 0 then None
    else
        while b >= 0 && b <> 10 do
            buf.Add(byte b)
            b <- stream.ReadByte()
        let arr = buf.ToArray()
        let len = if arr.Length > 0 && arr.[arr.Length - 1] = 13uy then arr.Length - 1 else arr.Length
        Some(Encoding.ASCII.GetString(arr, 0, len))

/// 本文を「文字数」ではなく「バイト数」ぴったり読み込む。
/// Content-Length は UTF-8 バイト数なので、マルチバイト文字を含む本文でも
/// フレーミングがずれないようにする（旧実装は char 単位で読みストリームが破綻していた）。
let private readBodyBytes (stream: Stream) (contentLength: int) : byte[] option =
    let buf = Array.zeroCreate contentLength
    let mutable offset = 0
    let mutable ok = true
    while ok && offset < contentLength do
        let read = stream.Read(buf, offset, contentLength - offset)
        if read <= 0 then ok <- false
        else offset <- offset + read
    if ok then Some buf else None

/// 指定ストリームから 1 メッセージを読み取る（バイト正確・UTF-8）。テスト可能。
let waitMessageFromStream (stream: Stream) : LSPMessage option =
    // ヘッダ行を空行（区切り）まで読む。先頭の空行は読み飛ばす。
    let headers = Dictionary<string, string>()
    let mutable sawHeader = false
    let mutable finished = false
    let mutable eof = false
    while not finished do
        match readHeaderLine stream with
        | None ->
            finished <- true
            eof <- true
        | Some line ->
            if line.Length = 0 then
                // 区切りの空行。ただしヘッダ未取得なら先頭空行として読み飛ばす。
                if sawHeader then finished <- true
            else
                sawHeader <- true
                match tryParseHeaderLine line with
                | Some(k, v) -> headers.[k] <- v
                | None -> ()

    if eof || headers.Count = 0 then None
    else
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
            match readBodyBytes stream len with
            | None -> None
            | Some bodyBytes ->
                try
                    let json = JObject.Parse(utf8NoBom.GetString bodyBytes)
                    if json.ContainsKey "id" then Some(RequestMsg(headers, json))
                    else Some(NotificationMsg(headers, json))
                with _ ->
                    None

/// 標準入力ストリーム（バッファ付き）。バイト単位で読むため Console のテキスト
/// API（InputEncoding 依存）は使わない。
let private stdinStream : Stream = new BufferedStream(Console.OpenStandardInput())

/// Block until a complete LSP message arrives on stdin, then return it.
/// Returns ``None`` if stream ended or message framing/content is invalid.
let waitMessage () : LSPMessage option =
    waitMessageFromStream stdinStream

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
