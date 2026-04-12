/// Types used for LSP protocol JSON serialization/deserialization.
module Atla.LanguageServer.LSPTypes

open Newtonsoft.Json

// ---------------------------------------------------------------------------
// Response payload types
// ---------------------------------------------------------------------------

type TextDocumentSyncKind =
    | None = 0
    | Full = 1
    | Incremental = 2

[<JsonObject>]
type TextDocumentSyncOptions(openClose: bool, change: TextDocumentSyncKind) =
    [<JsonProperty>]
    member _.openClose = openClose
    [<JsonProperty>]
    member _.change = change

[<JsonObject>]
type SemanticTokensLegend(tokenTypes: string list, tokenModifiers: string list) =
    [<JsonProperty>]
    member _.tokenTypes = tokenTypes
    [<JsonProperty>]
    member _.tokenModifiers = tokenModifiers

[<JsonObject>]
type SemanticTokensOptions(legend: SemanticTokensLegend, range: bool, full: bool) =
    [<JsonProperty>]
    member _.legend = legend
    [<JsonProperty>]
    member _.range = range
    [<JsonProperty>]
    member _.full = full

[<JsonObject>]
type ServerCapabilities
    (documentHighlightProvider: bool,
     textDocumentSync: TextDocumentSyncOptions,
     semanticTokensProvider: SemanticTokensOptions) =
    [<JsonProperty>]
    member _.documentHighlightProvider = documentHighlightProvider
    [<JsonProperty>]
    member _.textDocumentSync = textDocumentSync
    [<JsonProperty>]
    member _.semanticTokensProvider = semanticTokensProvider

[<JsonObject>]
type ServerInfo(name: string, version: string) =
    [<JsonProperty>]
    member _.name = name
    [<JsonProperty>]
    member _.version = version

[<JsonObject>]
type InitializeResult(capabilities: ServerCapabilities, serverInfo: ServerInfo) =
    [<JsonProperty>]
    member _.capabilities = capabilities
    [<JsonProperty>]
    member _.serverInfo = serverInfo

[<JsonObject>]
type Response(id: int, result: obj) =
    [<JsonProperty>]
    member _.id = id
    [<JsonProperty>]
    member _.result = result

[<JsonObject>]
type SemanticTokens(resultId: string, data: uint32 list) =
    [<JsonProperty(NullValueHandling = NullValueHandling.Ignore)>]
    member _.resultId = resultId
    [<JsonProperty>]
    member _.data = data

// ---------------------------------------------------------------------------
// Notification payload types
// ---------------------------------------------------------------------------

/// JSON payload for outgoing LSP notifications (``method`` + ``params``).
[<JsonObject>]
type LSPNotification(method: string, ``params``: obj) =
    [<JsonProperty>]
    member _.method = method
    [<JsonProperty("params")>]
    member _.``params`` = ``params``

type MessageType =
    | Error = 1
    | Warning = 2
    | Info = 3
    | Log = 4

[<JsonObject>]
type LogMessageParams(``type``: MessageType, message: string) =
    [<JsonProperty>]
    member _.``type`` = int ``type``
    [<JsonProperty>]
    member _.message = message

[<JsonObject>]
type Position(line: int, character: int) =
    [<JsonProperty>]
    member _.line = line
    [<JsonProperty>]
    member _.character = character

[<JsonObject>]
type Range(start: Position, ``end``: Position) =
    [<JsonProperty>]
    member _.start = start
    [<JsonProperty>]
    member _.``end`` = ``end``

[<JsonObject>]
type Diagnostic(range: Range, message: string) =
    [<JsonProperty>]
    member _.range = range
    [<JsonProperty>]
    member _.message = message

[<JsonObject>]
type PublishDiagnosticsParams(uri: string, diagnostics: Diagnostic list) =
    [<JsonProperty>]
    member _.uri = uri
    [<JsonProperty>]
    member _.diagnostics = diagnostics
