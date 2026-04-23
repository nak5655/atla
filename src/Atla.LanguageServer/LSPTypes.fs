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

/// LSP CompletionOptions – 補完プロバイダの設定。
[<JsonObject>]
type CompletionOptions(?triggerCharacters: string list) =
    [<JsonProperty(NullValueHandling = NullValueHandling.Ignore)>]
    member _.triggerCharacters = triggerCharacters |> Option.toObj

[<JsonObject>]
type ServerCapabilities
    (documentHighlightProvider: bool,
     textDocumentSync: TextDocumentSyncOptions,
     semanticTokensProvider: SemanticTokensOptions,
     ?completionProvider: CompletionOptions,
     ?hoverProvider: bool,
     ?definitionProvider: bool) =
    [<JsonProperty>]
    member _.documentHighlightProvider = documentHighlightProvider
    [<JsonProperty>]
    member _.textDocumentSync = textDocumentSync
    [<JsonProperty>]
    member _.semanticTokensProvider = semanticTokensProvider
    [<JsonProperty(NullValueHandling = NullValueHandling.Ignore)>]
    member _.completionProvider = completionProvider |> Option.toObj
    [<JsonProperty>]
    member _.hoverProvider = hoverProvider |> Option.defaultValue false
    [<JsonProperty>]
    member _.definitionProvider = definitionProvider |> Option.defaultValue false

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
type ErrorObject(code: int, message: string) =
    [<JsonProperty>]
    member _.code = code
    [<JsonProperty>]
    member _.message = message

[<JsonObject>]
type ErrorResponse(id: int, error: ErrorObject) =
    [<JsonProperty>]
    member _.id = id
    [<JsonProperty>]
    member _.error = error

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

type DiagnosticSeverity =
    | Error = 1
    | Warning = 2
    | Information = 3
    | Hint = 4

[<JsonObject>]
type Diagnostic
    (
        range: Range,
        message: string,
        ?severity: DiagnosticSeverity,
        ?source: string,
        ?code: string
    ) =
    [<JsonProperty>]
    member _.range = range
    [<JsonProperty>]
    member _.message = message
    [<JsonProperty(NullValueHandling = NullValueHandling.Ignore)>]
    member _.severity = severity |> Option.map int |> Option.map box |> Option.defaultValue null
    [<JsonProperty(NullValueHandling = NullValueHandling.Ignore)>]
    member _.source = source |> Option.toObj
    [<JsonProperty(NullValueHandling = NullValueHandling.Ignore)>]
    member _.code = code |> Option.toObj

[<JsonObject>]
type PublishDiagnosticsParams(uri: string, diagnostics: Diagnostic list) =
    [<JsonProperty>]
    member _.uri = uri
    [<JsonProperty>]
    member _.diagnostics = diagnostics

// ---------------------------------------------------------------------------
// IntelliSense payload types
// ---------------------------------------------------------------------------

/// LSP CompletionItemKind の一部。
type CompletionItemKind =
    | Text = 1
    | Method = 2
    | Function = 3
    | Field = 5
    | Variable = 6
    | Class = 7
    | Keyword = 14

/// LSP CompletionItem。
[<JsonObject>]
type CompletionItem(label: string, ?kind: CompletionItemKind, ?detail: string) =
    [<JsonProperty>]
    member _.label = label
    [<JsonProperty(NullValueHandling = NullValueHandling.Ignore)>]
    member _.kind = kind |> Option.map int |> Option.map box |> Option.defaultValue null
    [<JsonProperty(NullValueHandling = NullValueHandling.Ignore)>]
    member _.detail = detail |> Option.toObj

/// LSP CompletionList。
[<JsonObject>]
type CompletionList(isIncomplete: bool, items: CompletionItem list) =
    [<JsonProperty>]
    member _.isIncomplete = isIncomplete
    [<JsonProperty>]
    member _.items = items

/// LSP MarkupContent（Hover のコンテンツとして使用）。
[<JsonObject>]
type MarkupContent(kind: string, value: string) =
    [<JsonProperty>]
    member _.kind = kind
    [<JsonProperty>]
    member _.value = value

/// LSP Hover レスポンス。
[<JsonObject>]
type Hover(contents: MarkupContent, ?range: Range) =
    [<JsonProperty>]
    member _.contents = contents
    [<JsonProperty(NullValueHandling = NullValueHandling.Ignore)>]
    member _.range = range |> Option.toObj

/// LSP Location（定義ジャンプのレスポンスとして使用）。
[<JsonObject>]
type Location(uri: string, range: Range) =
    [<JsonProperty>]
    member _.uri = uri
    [<JsonProperty>]
    member _.range = range

