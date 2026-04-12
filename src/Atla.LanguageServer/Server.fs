/// Core server logic: initialize, tokenize (semantic tokens), and compile.
module Atla.LanguageServer.Server

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open System.Reflection
open Newtonsoft.Json.Linq
open Atla.Compiler
open Atla.Core.Data
open Atla.Core.Syntax
open Atla.Core.Syntax.Data
open Atla.LanguageServer.LSPTypes
open Atla.LanguageServer.LSPMessage

// ---------------------------------------------------------------------------
// Helper: convert an Atla.Core span to an LSP Range
// ---------------------------------------------------------------------------

let private spanToRange (span: Span) : Range =
    Range(
        Position(span.left.Line, span.left.Column),
        Position(span.right.Line, span.right.Column)
    )

let private sanitizeAssemblyName (value: string) : string =
    let chars = value |> Seq.map (fun c -> if Char.IsLetterOrDigit c then c else '_') |> Seq.toArray
    let sanitized = String(chars)
    if String.IsNullOrWhiteSpace sanitized then "Application" else sanitized

type private DiagnosticStage =
    | Lex
    | Parse
    | Semantic
    | Unknown

type private DiagnosticEnvelope =
    { stage: DiagnosticStage
      span: Span
      message: string
      code: string }

let private classifyDiagnosticStage (message: string) : DiagnosticStage =
    let m = if isNull message then "" else message.ToLowerInvariant()
    if m.Contains("lex") || m.Contains("token") then Lex
    elif m.Contains("parse") || m.Contains("syntax") || m.Contains("unsupported declaration type") then Parse
    elif m.Contains("type") || m.Contains("unresolved") || m.Contains("semantic") then Semantic
    else Unknown

let private toDiagnosticEnvelopes (compileError: string) : DiagnosticEnvelope list =
    let stage = classifyDiagnosticStage compileError
    let code =
        match stage with
        | Lex -> "ATLALS001"
        | Parse -> "ATLALS002"
        | Semantic -> "ATLALS003"
        | Unknown -> "ATLALS000"

    [ { stage = stage
        span = Span.Empty
        message = compileError
        code = code } ]

let private tryExtractSpanFromMessage (message: string) : Span option =
    if String.IsNullOrWhiteSpace message then
        None
    else
        let compact = message.Replace("\r", " ").Replace("\n", " ")
        let lineColPattern = Regex(@"at\s+(?<line>\d+):(?<col>\d+)", RegexOptions.IgnoreCase)
        let lineColumnPattern = Regex(@"line\s*=\s*(?<line>\d+)\s*;?\s*column\s*=\s*(?<col>\d+)", RegexOptions.IgnoreCase)

        let tryCreateSpan (m: Match) =
            if not m.Success then
                None
            else
                let okLine, line = Int32.TryParse m.Groups.["line"].Value
                let okCol, col = Int32.TryParse m.Groups.["col"].Value
                if okLine && okCol then
                    let left: Atla.Core.Data.Position = { Line = line; Column = col }
                    let right: Atla.Core.Data.Position = { Line = line; Column = col + 1 }
                    Some({ left = left; right = right })
                else
                    None

        match tryCreateSpan (lineColPattern.Match compact) with
        | Some span -> Some span
        | None -> tryCreateSpan (lineColumnPattern.Match compact)

let private toLspDiagnostics (envelopes: DiagnosticEnvelope list) : Diagnostic list =
    envelopes
    |> List.mapi (fun i x ->
        let span =
            match tryExtractSpanFromMessage x.message with
            | Some parsed -> parsed
            | None -> x.span

        let severity =
            match x.stage with
            | Lex
            | Parse
            | Semantic
            | Unknown -> DiagnosticSeverity.Error

        i, Diagnostic(spanToRange span, x.message, severity = severity, source = "atla-lsp", code = x.code))
    |> List.sortBy (fun (i, d) ->
        d.range.start.line,
        d.range.start.character,
        d.range.``end``.line,
        d.range.``end``.character,
        d.message,
        i)
    |> List.map snd


let private normalizePathForKey (path: string) : string =
    let full = Path.GetFullPath(path).Replace('\\', '/')
    if Path.DirectorySeparatorChar = '\\' then full.ToLowerInvariant() else full

let private tryNormalizeUri (uriText: string) : string option =
    if String.IsNullOrWhiteSpace uriText then None
    else
        let mutable u = Unchecked.defaultof<Uri>
        if Uri.TryCreate(uriText, UriKind.Absolute, &u) then
            if u.IsFile then
                let normalizedPath = normalizePathForKey u.LocalPath
                Some(sprintf "file://%s" normalizedPath)
            else
                Some(uriText.Trim())
        else
            None

let private pathIsUnder (candidatePath: string) (rootPath: string) : bool =
    candidatePath = rootPath || candidatePath.StartsWith(rootPath + "/", StringComparison.Ordinal)

let private tryUriToNormalizedPath (uriText: string) : string option =
    let mutable u = Unchecked.defaultof<Uri>
    if Uri.TryCreate(uriText, UriKind.Absolute, &u) && u.IsFile then
        Some(normalizePathForKey u.LocalPath)
    else
        None

let private collectWorkspaceRoots (content: JObject) : string list =
    let rootsFromFolders =
        match content.SelectToken("$.params.workspaceFolders") with
        | :? JArray as folders ->
            folders
            |> Seq.choose (fun x ->
                match x.["uri"] with
                | null -> None
                | value -> value.ToString() |> tryUriToNormalizedPath)
            |> Seq.toList
        | _ -> []

    let rootFromRootUri =
        match content.SelectToken("$.params.rootUri") with
        | null -> None
        | value -> value.ToString() |> tryUriToNormalizedPath

    match rootsFromFolders with
    | _ :: _ -> rootsFromFolders
    | [] ->
        match rootFromRootUri with
        | Some root -> [ root ]
        | None -> []

// ---------------------------------------------------------------------------
// Server
// ---------------------------------------------------------------------------

/// Mutable server state (one instance per process).
type Server(?publishDiagnosticsFn: (string -> Diagnostic list -> unit)) =

    // ---- persistent state --------------------------------------------------
    let mutable isAvailablePublishDiagnostics = false
    let mutable tokenTypes: string[] = [||]
    let mutable workspaceRoots: string list = []
    let buffers = Dictionary<string, string>()
    let displayUris = Dictionary<string, string>()
    let publish = defaultArg publishDiagnosticsFn publishDiagnostics

    let canCompileUri (normalizedUri: string) : bool =
        match tryUriToNormalizedPath normalizedUri with
        | None -> false
        | Some path ->
            match workspaceRoots with
            | [] -> true
            | roots -> roots |> List.exists (pathIsUnder path)

    let compileAndPublish (normalizedUri: string) (displayUri: string) (text: string) =
        if isAvailablePublishDiagnostics && canCompileUri normalizedUri then
            let outputDir = Path.Combine(Path.GetTempPath(), "atla-lsp")
            Directory.CreateDirectory(outputDir) |> ignore

            let asmName =
                if String.IsNullOrWhiteSpace normalizedUri then "Application"
                else normalizedUri |> Path.GetFileNameWithoutExtension |> sanitizeAssemblyName

            try
                match Compiler.compile(asmName, text, outputDir) with
                | Ok () -> publish displayUri []
                | Error message ->
                    message |> toDiagnosticEnvelopes |> toLspDiagnostics |> publish displayUri
            with ex ->
                let fallback =
                    sprintf "Compiler internal error: %s" ex.Message
                    |> toDiagnosticEnvelopes
                    |> toLspDiagnostics

                publish displayUri fallback

    // ---- public surface used by tests --------------------------------------
    member _.IsAvailablePublishDiagnostics
        with get() = isAvailablePublishDiagnostics
        and set(v) = isAvailablePublishDiagnostics <- v

    member _.TokenTypes
        with get() = tokenTypes
        and set(v) = tokenTypes <- v

    member _.WorkspaceRoots
        with get() = workspaceRoots

    member _.TryNormalizeUri(uri: string) =
        tryNormalizeUri uri

    // ---- initialize --------------------------------------------------------

    /// Handle the LSP ``initialize`` request. Returns the ``InitializeResult``
    /// payload that should be sent back to the client.
    member _.Initialize(content: JObject) : InitializeResult =

        // Check whether the client supports publishDiagnostics.
        isAvailablePublishDiagnostics <-
            try
                content.["params"].["capabilities"].["textDocument"].["publishDiagnostics"].["relatedInformation"]
                    .ToString().ToLower() = "true"
            with _ -> false

        workspaceRoots <- collectWorkspaceRoots content

        // Intersect client-supported token types with server-supported ones.
        let clientTokenTypes =
            try
                content.["params"].["capabilities"].["textDocument"].["semanticTokens"].["tokenTypes"].ToObject<string[]>()
            with _ -> [||]

        let serverTokenTypes = [| "keyword"; "comment"; "string"; "number"; "variable"; "type" |]
        tokenTypes <- serverTokenTypes |> Array.filter (fun t -> clientTokenTypes |> Array.contains t)

        let capabilities =
            ServerCapabilities(
                false,
                TextDocumentSyncOptions(true, TextDocumentSyncKind.Full),
                SemanticTokensOptions(
                    SemanticTokensLegend(tokenTypes |> Array.toList, []),
                    false,
                    true
                )
            )

        let assembly = Assembly.GetExecutingAssembly()
        let versionInfo = FileVersionInfo.GetVersionInfo(assembly.Location)
        let serverInfo = ServerInfo("atla-lsp", versionInfo.FileVersion)

        InitializeResult(capabilities, serverInfo)

    // ---- semantic tokens ---------------------------------------------------

    /// Return semantic token data for the document at ``uri``, or ``[]`` if
    /// the document has not been opened yet.
    member this.Tokenize(uri: string) : uint32 list =
        match tryNormalizeUri uri with
        | None -> []
        | Some key ->
            match buffers.TryGetValue key with
            | false, _ -> []
            | true, text -> this.InternalTokenize text

    /// Tokenize ``text`` and produce the flat encoded token list defined by
    /// the LSP semantic-tokens specification.
    member _.InternalTokenize(text: string) : uint32 list =
        let input: Input<SourceChar> = StringInput(text)
        match Lexer.tokenize input Position.Zero with
        | Success(tokens, _) ->
            let mutable line = 0
            let mutable col = 0
            let data = ResizeArray<uint32>()

            for token in tokens do
                let span = token.span
                let tline = span.left.Line
                let tcol = span.left.Column
                let tlen = max 0 (span.right.Column - span.left.Column)

                let tokenType =
                    match token with
                    | :? Token.Keyword -> Some "keyword"
                    | :? Token.Int
                    | :? Token.Float -> Some "number"
                    | :? Token.String -> Some "string"
                    | :? Token.Id as id ->
                        if not (String.IsNullOrEmpty id.str) && Char.IsUpper(id.str.[0]) then Some "type" else Some "variable"
                    | _ -> None

                let dline = max 0 (tline - line)
                let dcol =
                    if dline = 0 then max 0 (tcol - col)
                    else max 0 tcol

                match tokenType with
                | Some tt when tokenTypes |> Array.contains tt ->
                    let idx = Array.IndexOf(tokenTypes, tt)
                    data.AddRange([| uint32 dline; uint32 dcol; uint32 tlen; uint32 idx; 0u |])
                    line <- tline
                    col <- tcol
                | _ -> ()

            data |> Seq.toList
        | Failure _ -> []

    // ---- document lifecycle / compile / diagnostics ------------------------

    member _.OpenDocument(uri: string, text: string) =
        match tryNormalizeUri uri with
        | None -> ()
        | Some key ->
            buffers.[key] <- text
            displayUris.[key] <- uri
            compileAndPublish key uri text

    member _.ChangeDocument(uri: string, text: string) =
        match tryNormalizeUri uri with
        | None -> ()
        | Some key ->
            buffers.[key] <- text
            displayUris.[key] <- uri
            compileAndPublish key uri text

    member _.CloseDocument(uri: string) =
        match tryNormalizeUri uri with
        | None -> ()
        | Some key ->
            if isAvailablePublishDiagnostics then
                publish uri []

            buffers.Remove(key) |> ignore
            displayUris.Remove(key) |> ignore

    /// Backward-compatible entrypoint for existing callers.
    member this.Compile(uri: string, text: string) =
        this.ChangeDocument(uri, text)
