/// Core server logic: initialize, tokenize (semantic tokens), and compile.
module Atla.LanguageServer.Server

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Reflection
open Newtonsoft.Json.Linq
open Atla.Core
open Atla.Compiler
open Atla.LanguageServer.LSPTypes
open Atla.LanguageServer.LSPMessage

// ---------------------------------------------------------------------------
// Helper: convert an Atla.Lang span to an LSP Range
// ---------------------------------------------------------------------------

let private spanToRange (span: Atla.Core.Data.Span) : Range =
    Range(
        Position(span.left.Line, span.left.Column),
        Position(span.right.Line, span.right.Column)
    )

// ---------------------------------------------------------------------------
// Server
// ---------------------------------------------------------------------------

/// Mutable server state (one instance per process).
type Server() =

    // ---- persistent state --------------------------------------------------
    let mutable isAvailablePublishDiagnostics = false
    let mutable tokenTypes: string[] = [||]
    let buffers = Dictionary<string, string>()
    let mutable compiler: Atla.Compiler option = None
    let mutable asm: Atla.Core.Semantics.Data.Hir.Assembly option = None
    let compileProblems = ResizeArray<Atla.Core.Problem>()

    // ---- public surface used by tests --------------------------------------
    member _.IsAvailablePublishDiagnostics
        with get()  = isAvailablePublishDiagnostics
        and  set(v) = isAvailablePublishDiagnostics <- v

    member _.TokenTypes
        with get()  = tokenTypes
        and  set(v) = tokenTypes <- v

    // ---- initialize --------------------------------------------------------

    /// Handle the LSP ``initialize`` request.  Returns the ``InitializeResult``
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
                content.["params"].["capabilities"].["textDocument"].["semanticTokens"].["tokenTypes"].ToArray()
                |> Array.map (fun t -> t.ToString())
            with _ -> [||]

        let serverTokenTypes = [| "keyword"; "comment"; "string"; "number"; "variable"; "type" |]
        tokenTypes <- serverTokenTypes |> Array.filter (fun t -> clientTokenTypes.Contains t)

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

        let assembly   = Assembly.GetExecutingAssembly()
        let versionInfo = FileVersionInfo.GetVersionInfo(assembly.Location)
        let serverInfo  = ServerInfo("atla-lsp", versionInfo.FileVersion)

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
    ///
    /// Exposed as a non-private member so that unit tests can call it
    /// directly (mirrors ``_tokenize`` in the original Nemerle code).
    member _.InternalTokenize(text: string) : uint32 list =
        let lexer = Atla.Core.Syntax.Lexer()
        match lexer.tokenize text with
        | :? Atla.Lang.Parser.Result.Success as s ->
            // Nemerle variant Success { value1: tokens; value2: remainder }
            // Enumerate the tokens (first field of the Success case).
            let tokens = s.value1 :?> seq<Atla.Lang.Parser.Token>

            let mutable line = 0
            let mutable col  = 0
            let data = ResizeArray<uint32>()

            for token in tokens do
                let tline = token.span.lo.line
                let tcol  = token.span.lo.col
                let mutable tlen  = token.span.hi.index - token.span.lo.index

                // Map each token to one of the LSP semantic-token type strings.
                let tokenType =
                    match token with
                    | :? Atla.Lang.Parser.Token.Delim as d
                        when Char.IsLetter(d.s.[0]) -> Some "keyword"
                    | :? Atla.Lang.Parser.Token.Comment -> Some "comment"
                    | :? Atla.Lang.Parser.Token.Int     -> Some "number"
                    | :? Atla.Lang.Parser.Token.String  -> Some "string"
                    | :? Atla.Lang.Parser.Token.Id as id ->
                        if Char.IsUpper(id.s.[0]) then Some "type" else Some "variable"
                    | _ -> None

                // Compute deltas relative to the previous token position.
                let mutable dline = tline - line
                let mutable dcol  = if dline = 0 then tcol - col else tcol

                // Guard against negative/illegal spans (log and clamp to 0).
                if tlen < 0 then
                    windowLogMessage (sprintf "%A has an illegal span: %A" token token.span) MessageType.Log
                    tlen <- 0
                if dline < 0 then
                    windowLogMessage (sprintf "%A has an illegal span: %A" token token.span) MessageType.Log
                    dline <- 0
                if dcol < 0 then
                    windowLogMessage (sprintf "%A has an illegal span: %A" token token.span) MessageType.Log
                    dcol <- 0

                match tokenType with
                | Some tt when tokenTypes.Contains tt ->
                    let idx = Array.IndexOf(tokenTypes, tt)
                    data.AddRange [| uint32 dline; uint32 dcol; uint32 tlen; uint32 idx; 0u |]
                    line <- tline
                    col  <- tcol
                | _ -> ()

            data |> Seq.toList

        | _ -> []

    // ---- compile / diagnostics ---------------------------------------------

    /// Store the document text for ``uri`` and, if ``publishDiagnostics`` is
    /// available, compile it and push diagnostics to the client.
    member this.Compile(uri: string, text: string) =
        buffers.[uri] <- text

        if not isAvailablePublishDiagnostics then ()
        else

        // Lazily create the compiler the first time we need it.
        if compiler.IsNone then
            compiler <- Some(Atla.Core.Compiler(fun problem -> compileProblems.Add problem))

        // Lazily create the assembly the first time we need it.
        if asm.IsNone then
            let newAsm = Atla.Core.Semantics.Data.Hir.Assembly("Application", "Application.exe")
            newAsm.scope <- Atla.Core.Semantics.Data.Hir.Scope.Assembly(Atla.Core.Semantics.Data.Hir.Scope.Global(), "Application")
            asm <- Some newAsm

        // Strip the ``file:///`` prefix (and Windows drive letter if present)
        // to get the absolute POSIX path, then make it relative to cwd.
        //   Original: uri.Replace('\\','/').Skip(8).SkipWhile(c => c != '/').ToArray()
        let filePathStr =
            String(uri.Replace('\\', '/').Skip(8).SkipWhile(fun c -> c <> '/').ToArray())

        let root =
            String(Directory.GetCurrentDirectory().Replace('\\', '/').SkipWhile(fun c -> c <> '/').ToArray())

        if filePathStr.StartsWith root then
            let rel     = filePathStr.[root.Length ..]
            let parts   = rel.Split(Path.DirectorySeparatorChar) : string[]
            let modPath = String.Join(Atla.Lang.Consts.MODULE_SEP, parts)
            compiler.Value.updateModule(asm.Value, modPath, text) |> ignore
        else
            publishDiagnostics uri
                [ Diagnostic(
                    spanToRange (Atla.Core.Data.Span.Empty),
                    sprintf "filePath is not a sub path of the root directory at %s." root) ]
