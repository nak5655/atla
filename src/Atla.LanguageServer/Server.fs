/// Core server logic: initialize, tokenize (semantic tokens), and compile.
module Atla.LanguageServer.Server

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
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

// ---------------------------------------------------------------------------
// Server
// ---------------------------------------------------------------------------

/// Mutable server state (one instance per process).
type Server() =

    // ---- persistent state --------------------------------------------------
    let mutable isAvailablePublishDiagnostics = false
    let mutable tokenTypes: string[] = [||]
    let buffers = Dictionary<string, string>()

    // ---- public surface used by tests --------------------------------------
    member _.IsAvailablePublishDiagnostics
        with get() = isAvailablePublishDiagnostics
        and set(v) = isAvailablePublishDiagnostics <- v

    member _.TokenTypes
        with get() = tokenTypes
        and set(v) = tokenTypes <- v

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

        // Intersect client-supported token types with server-supported ones.
        let clientTokenTypes =
            try
                content.["params"].["capabilities"].["textDocument"].["semanticTokens"].["tokenTypes"].ToObject<string[]>()
            with _ -> [||]

        let serverTokenTypes = [| "keyword"; "comment"; "string"; "number"; "variable"; "type" |]
        tokenTypes <- serverTokenTypes |> Array.filter (fun t -> clientTokenTypes |> Array.contains t)

        let capabilities =
            ServerCapabilities(
                true,
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
        match buffers.TryGetValue uri with
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

    // ---- compile / diagnostics ---------------------------------------------

    /// Store the document text for ``uri`` and, if ``publishDiagnostics`` is
    /// available, compile it and push diagnostics to the client.
    member _.Compile(uri: string, text: string) =
        buffers.[uri] <- text

        if isAvailablePublishDiagnostics then
            let outputDir = Path.Combine(Path.GetTempPath(), "atla-lsp")
            Directory.CreateDirectory(outputDir) |> ignore

            let asmName =
                if String.IsNullOrWhiteSpace uri then "Application"
                else uri |> Path.GetFileNameWithoutExtension |> sanitizeAssemblyName

            match Compiler.compile(asmName, text, outputDir) with
            | Ok () -> publishDiagnostics uri []
            | Error message ->
                publishDiagnostics uri [ Diagnostic(spanToRange Span.Empty, message) ]
