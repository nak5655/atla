/// Core server logic: initialize, tokenize (semantic tokens), and compile.
module Atla.LanguageServer.Server

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open Newtonsoft.Json.Linq
open Atla.Compiler
open Atla.Build
open Atla.Core.Data
open Atla.Core.Semantics.Data
open Atla.Core.Semantics
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
    let sanitized = System.String(chars)
    if String.IsNullOrWhiteSpace sanitized then "Application" else sanitized

let private canonicalTokenTypes = [| "keyword"; "type"; "variable"; "number"; "string" |]

let private normalizeSemanticInput (text: string) : string =
    let raw = if isNull text then "" else text
    let withoutBom =
        if raw.Length > 0 && raw.[0] = '\uFEFF' then raw.Substring(1) else raw

    withoutBom.Replace("\r\n", "\n").Replace("\r", "\n")

let private toLspSeverity (severity: Atla.Core.Semantics.Data.DiagnosticSeverity) : DiagnosticSeverity =
    match severity with
    | Atla.Core.Semantics.Data.DiagnosticSeverity.Error -> DiagnosticSeverity.Error
    | Atla.Core.Semantics.Data.DiagnosticSeverity.Warning -> DiagnosticSeverity.Warning
    | Atla.Core.Semantics.Data.DiagnosticSeverity.Info -> DiagnosticSeverity.Information

let private toLspDiagnostics
    (source: string)
    (diagnostics: Atla.Core.Semantics.Data.Diagnostic list)
    : Atla.LanguageServer.LSPTypes.Diagnostic list =
    diagnostics
    |> List.mapi (fun i x ->
        let span = x.span
        let severity = toLspSeverity x.severity
        i, Diagnostic(spanToRange span, x.message, severity = severity, source = source))
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

let private resolveServerVersion (assemblyLocation: string) : string =
    if String.IsNullOrWhiteSpace assemblyLocation then
        "0.0.0"
    else
        try
            let versionInfo = FileVersionInfo.GetVersionInfo(assemblyLocation)
            if String.IsNullOrWhiteSpace versionInfo.FileVersion then "0.0.0" else versionInfo.FileVersion
        with _ ->
            "0.0.0"

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

let private isWithinWorkspaceRoots (workspaceRoots: string list) (path: string) : bool =
    match workspaceRoots with
    | [] -> true
    | roots -> roots |> List.exists (pathIsUnder path)

let private tryFindProjectRootFromManifest (workspaceRoots: string list) (documentPath: string) : string option =
    /// Walk from the document parent directory toward ancestors until atla.yaml is found.
    let rec loop (currentDir: string) : string option =
        let manifestPath = Path.Join(currentDir, "atla.yaml")

        if File.Exists manifestPath then
            Some currentDir
        else
            let parent = Directory.GetParent(currentDir)

            if isNull parent then
                None
            elif isWithinWorkspaceRoots workspaceRoots (normalizePathForKey parent.FullName) then
                loop (normalizePathForKey parent.FullName)
            else
                None

    let docDir = Path.GetDirectoryName(documentPath)

    if String.IsNullOrWhiteSpace docDir then
        None
    else
        loop (normalizePathForKey docDir)

// ---------------------------------------------------------------------------
// Server
// ---------------------------------------------------------------------------

/// Mutable server state (one instance per process).
type Server
    (
        ?publishDiagnosticsFn: (string -> Atla.LanguageServer.LSPTypes.Diagnostic list -> unit),
        ?assemblyLocationResolver: unit -> string,
        ?buildProjectFn: BuildRequest -> BuildResult,
        ?compileFn: Compiler.CompileRequest -> Compiler.CompileResult
    ) =

    // ---- persistent state --------------------------------------------------
    let mutable isAvailablePublishDiagnostics = false
    let mutable tokenTypes: string[] = [||]
    let mutable semanticTokensFallbackReason: string option = None
    let mutable workspaceRoots: string list = []
    let buffers = Dictionary<string, string>()
    let displayUris = Dictionary<string, string>()
    /// キャッシュ: 正規化 URI → (PositionIndex, SymbolTable)。
    /// コンパイルが意味解析フェーズを通過した場合に格納され、IntelliSense クエリに使用する。
    let hirCache = Dictionary<string, PositionIndex.PositionIndex * SymbolTable>()
    let publish = defaultArg publishDiagnosticsFn publishDiagnostics
    let getAssemblyLocation =
        defaultArg assemblyLocationResolver (fun () -> Assembly.GetExecutingAssembly().Location)
    let buildProject = defaultArg buildProjectFn BuildSystem.buildProject
    let compile = defaultArg compileFn Compiler.compile

    let canCompileUri (normalizedUri: string) : bool =
        match tryUriToNormalizedPath normalizedUri with
        | None -> false
        | Some path ->
            match workspaceRoots with
            | [] -> true
            | roots -> roots |> List.exists (pathIsUnder path)

    let resolveDependenciesForDocument
        (normalizedUri: string)
        : Result<Compiler.ResolvedDependency list, Atla.Core.Semantics.Data.Diagnostic list> =
        match tryUriToNormalizedPath normalizedUri with
        | None -> Ok []
        | Some normalizedPath ->
            match tryFindProjectRootFromManifest workspaceRoots normalizedPath with
            | None -> Ok []
            | Some projectRoot ->
                let buildResult = buildProject { projectRoot = projectRoot }
                if buildResult.succeeded then
                    buildResult.plan
                    |> Option.map (fun plan -> plan.dependencies)
                    |> Option.defaultValue []
                    |> Ok
                else
                    Result.Error buildResult.diagnostics

    let compileAndPublish (normalizedUri: string) (displayUri: string) (text: string) =
        if isAvailablePublishDiagnostics && canCompileUri normalizedUri then
            let outputDir = Path.Combine(Path.GetTempPath(), "atla-lsp")
            Directory.CreateDirectory(outputDir) |> ignore

            let asmName =
                if String.IsNullOrWhiteSpace normalizedUri then "Application"
                else normalizedUri |> Path.GetFileNameWithoutExtension |> sanitizeAssemblyName

            try
                match resolveDependenciesForDocument normalizedUri with
                | Result.Error buildDiagnostics ->
                    buildDiagnostics |> toLspDiagnostics "atla-build" |> publish displayUri
                | Ok dependencies ->
                    let compileResult =
                        compile { asmName = asmName; source = text; outDir = outputDir; dependencies = dependencies }

                    // 意味解析が成功した場合は HIR からインデックスを構築してキャッシュする。
                    match compileResult.hir, compileResult.symbolTable with
                    | Some hirAsm, Some symTable ->
                        let index = PositionIndex.build hirAsm
                        hirCache[normalizedUri] <- (index, symTable)
                    | _ ->
                        hirCache.Remove normalizedUri |> ignore

                    compileResult.diagnostics |> toLspDiagnostics "atla-compiler" |> publish displayUri
            with ex ->
                let fallback =
                    [ Atla.Core.Semantics.Data.Diagnostic.Error(sprintf "Compiler internal error: %s" ex.Message, Span.Empty) ]
                    |> toLspDiagnostics "atla-lsp"

                publish displayUri fallback
        elif isAvailablePublishDiagnostics then
            publish displayUri []

    // ---- public surface used by tests --------------------------------------
    member _.IsAvailablePublishDiagnostics
        with get() = isAvailablePublishDiagnostics
        and set(v) = isAvailablePublishDiagnostics <- v

    member _.TokenTypes
        with get() = tokenTypes
        and set(v) = tokenTypes <- v

    member _.SemanticTokensFallbackReason
        with get() = semanticTokensFallbackReason

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

        let supported = canonicalTokenTypes |> Array.filter (fun t -> clientTokenTypes |> Array.contains t)
        tokenTypes <- supported
        semanticTokensFallbackReason <-
            if clientTokenTypes.Length = 0 then
                Some("Semantic tokens disabled: client did not advertise semantic token types.")
            elif supported.Length = 0 then
                Some("Semantic tokens disabled: no overlap between client token types and server token legend.")
            else
                None

        let capabilities =
            ServerCapabilities(
                false,
                TextDocumentSyncOptions(true, TextDocumentSyncKind.Full),
                SemanticTokensOptions(
                    SemanticTokensLegend(tokenTypes |> Array.toList, []),
                    false,
                    true
                ),
                CompletionOptions([ "." ]),
                true,
                true
            )

        let serverVersion = getAssemblyLocation () |> resolveServerVersion
        let serverInfo = ServerInfo("atla-lsp", serverVersion)

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
        let inputText = normalizeSemanticInput text
        let input: Input<SourceChar> = StringInput(inputText)
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
            hirCache.Remove(key) |> ignore

    /// Backward-compatible entrypoint for existing callers.
    member this.Compile(uri: string, text: string) =
        this.ChangeDocument(uri, text)

    // ---- IntelliSense ------------------------------------------------------

    /// 指定した URI の PositionIndex とシンボルテーブルをキャッシュから取得する。
    member private _.TryGetIndex(uri: string) : (PositionIndex.PositionIndex * SymbolTable) option =
        match tryNormalizeUri uri with
        | None -> None
        | Some key ->
            match hirCache.TryGetValue key with
            | true, entry -> Some entry
            | false, _ -> None

    /// 補完候補リストを返す。
    /// モジュールスコープ内の全シンボルを候補として提供する。
    member this.GetCompletions(uri: string, _line: int, _character: int) : CompletionList =
        match this.TryGetIndex uri with
        | None -> CompletionList(false, [])
        | Some(index, symbolTable) ->
            let visibleVars = index.moduleScope.allVisibleVars()
            let items =
                visibleVars
                |> List.choose (fun (name, sid) ->
                    match symbolTable.Get sid with
                    | None -> None
                    | Some symInfo ->
                        let kind =
                            match symInfo.kind with
                            | SymbolKind.BuiltinOperator _ -> Some CompletionItemKind.Function
                            | SymbolKind.Local _ -> Some CompletionItemKind.Variable
                            | SymbolKind.Arg _ -> Some CompletionItemKind.Variable
                            | SymbolKind.External(ExternalBinding.NativeMethodGroup _) -> Some CompletionItemKind.Method
                            | SymbolKind.External(ExternalBinding.ConstructorGroup _) -> Some CompletionItemKind.Class
                            | SymbolKind.External(ExternalBinding.SystemTypeRef _) -> Some CompletionItemKind.Class
                        let detail = PositionIndex.formatTypeWithTable symbolTable symInfo.typ
                        Some(CompletionItem(name, ?kind = kind, detail = detail)))
            CompletionList(false, items)

    /// カーソル位置にある識別子のホバー情報を返す。
    member this.GetHover(uri: string, line: int, character: int) : Hover option =
        match this.TryGetIndex uri with
        | None -> None
        | Some(index, symbolTable) ->
            match PositionIndex.tryFindSymbolAt index line character with
            | None -> None
            | Some sid ->
                match symbolTable.Get sid with
                | None -> None
                | Some symInfo ->
                    let typeStr = PositionIndex.formatTypeWithTable symbolTable symInfo.typ
                    let mdText = sprintf "```\n%s: %s\n```" symInfo.name typeStr
                    Some(Hover(MarkupContent("markdown", mdText)))

    /// カーソル位置にある識別子の定義位置を返す。
    member this.GetDefinition(uri: string, line: int, character: int) : Location option =
        match this.TryGetIndex uri with
        | None -> None
        | Some(index, _) ->
            match PositionIndex.tryFindSymbolAt index line character with
            | None -> None
            | Some sid ->
                match PositionIndex.tryFindDeclSpan index sid with
                | None -> None
                | Some span ->
                    // 定義の URI は現在のドキュメントと同一と仮定する（単一ファイル）。
                    let displayUri =
                        match tryNormalizeUri uri with
                        | Some key ->
                            match displayUris.TryGetValue key with
                            | true, du -> du
                            | false, _ -> uri
                        | None -> uri
                    let range =
                        Range(
                            Position(span.left.Line, span.left.Column),
                            Position(span.right.Line, span.right.Column)
                        )
                    Some(Location(displayUri, range))
