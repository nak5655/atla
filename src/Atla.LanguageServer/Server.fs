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

/// On Windows, <c>Uri.LocalPath</c> retains a spurious leading <c>/</c> when the
/// drive-letter colon is percent-encoded in the original URI (e.g. <c>d%3A</c> → <c>/d:/</c>
/// instead of the expected <c>d:/</c>).  Strip that slash so the path can be handed
/// to <c>Path.GetFullPath</c> without producing garbage.
/// Checking the first three characters is sufficient: <c>/</c> then an ASCII letter then <c>:</c>
/// uniquely identifies a Windows drive-letter prefix regardless of what follows.
let private fixWindowsLocalPath (localPath: string) : string =
    if Path.DirectorySeparatorChar = '\\' &&
       localPath.Length >= 3 &&
       localPath.[0] = '/' &&
       Char.IsLetter(localPath.[1]) &&
       localPath.[2] = ':' then
        localPath.Substring(1)
    else
        localPath

let private tryNormalizeUri (uriText: string) : string option =
    if String.IsNullOrWhiteSpace uriText then None
    else
        let mutable u = Unchecked.defaultof<Uri>
        if Uri.TryCreate(uriText, UriKind.Absolute, &u) then
            if u.IsFile then
                let normalizedPath = u.LocalPath |> fixWindowsLocalPath |> normalizePathForKey
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
        Some(u.LocalPath |> fixWindowsLocalPath |> normalizePathForKey)
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
// IntelliSense ヘルパー関数
// ---------------------------------------------------------------------------

/// バッファテキストと補完トリガー位置を受け取り、直前がピリオドであれば
/// ピリオド前の識別子の名前・行・開始列を返す。
/// 返り値: Some(identName, identLine, identStartCol) または None（ドット補完でない場合）
let private detectDotContext (text: string) (line: int) (character: int) : (string * int * int) option =
    if line < 0 || character < 1 then None
    else
        let lines = text.Split('\n')
        if line >= lines.Length then None
        else
            let lineText = lines.[line]
            // character は補完トリガー位置（ピリオドの次の文字の列番号）。
            // character - 1 がピリオドであることを確認する。
            if character - 1 >= lineText.Length || lineText.[character - 1] <> '.' then None
            else
                let dotPos = character - 1
                // dotPos の手前から英数字・アンダースコアをスキャンして識別子の開始列を求める。
                let rec scanBack i =
                    if i < 0 then 0
                    elif Char.IsLetterOrDigit(lineText.[i]) || lineText.[i] = '_' then scanBack (i - 1)
                    else i + 1
                let identStart =
                    if dotPos = 0 then dotPos
                    else scanBack (dotPos - 1)
                if identStart >= dotPos then None
                else
                    let identName = lineText.[identStart .. dotPos - 1]
                    if String.IsNullOrEmpty identName then None
                    else Some(identName, line, identStart)

/// TypeId に対応するメンバー補完候補リストを返す。
/// TypeId.Native および TypeId.Name（インポート済み .NET 型）はリフレクションでメンバーを列挙し、
/// ユーザー定義型（TypeId.Name で typeFields に登録されているもの）はフィールドを返す。
let private getMembersOfType
    (symbolTable: SymbolTable)
    (index: PositionIndex.PositionIndex)
    (tid: TypeId)
    : CompletionItem list =
    let flags = BindingFlags.Public ||| BindingFlags.Instance
    // TypeId を .NET System.Type へ解決する。
    // TypeId.Native・組み込みスカラー型・インポート済み .NET 型名を処理する。
    let resolveSysType (t: TypeId) : System.Type option =
        match t with
        | TypeId.Native sysType -> Some sysType
        | TypeId.String          -> Some typeof<string>
        | TypeId.Int             -> Some typeof<int>
        | TypeId.Bool            -> Some typeof<bool>
        | TypeId.Float           -> Some typeof<float>
        | TypeId.Name sid ->
            match symbolTable.Get sid with
            | Some { kind = SymbolKind.External(ExternalBinding.SystemTypeRef sysType) }
                when not (obj.ReferenceEquals(sysType, null)) -> Some sysType
            | _ -> None
        | _ -> None
    match resolveSysType tid with
    | Some sysType ->
        // .NET 型：リフレクションでパブリックインスタンスメンバーを列挙する。
        [
            // プロパティ
            for p in sysType.GetProperties(flags) do
                let typeStr = PositionIndex.formatTypeWithTable symbolTable (TypeId.fromSystemType p.PropertyType)
                yield CompletionItem(p.Name, kind = CompletionItemKind.Field, detail = typeStr)
            // メソッド（特殊メソッドを除く。同名の多重定義は最初のシグネチャのみ掲載）
            let seenMethods = HashSet<string>()
            for m in sysType.GetMethods(flags) do
                if not m.IsSpecialName && seenMethods.Add(m.Name) then
                    let paramStr =
                        m.GetParameters()
                        |> Array.map (fun p ->
                            let ts = PositionIndex.formatTypeWithTable symbolTable (TypeId.fromSystemType p.ParameterType)
                            sprintf "%s: %s" p.Name ts)
                        |> String.concat ", "
                    let retStr = PositionIndex.formatTypeWithTable symbolTable (TypeId.fromSystemType m.ReturnType)
                    let detail = sprintf "(%s) -> %s" paramStr retStr
                    yield CompletionItem(m.Name, kind = CompletionItemKind.Method, detail = detail)
            // フィールド
            for f in sysType.GetFields(flags) do
                let typeStr = PositionIndex.formatTypeWithTable symbolTable (TypeId.fromSystemType f.FieldType)
                yield CompletionItem(f.Name, kind = CompletionItemKind.Field, detail = typeStr)
        ]
    | None ->
        // ユーザー定義型：typeFields インデックスからフィールド SymbolId を取得してシンボル名・型を返す。
        match tid with
        | TypeId.Name sid ->
            PositionIndex.tryGetTypeFieldIds index sid
            |> List.choose (fun fieldSid ->
                match symbolTable.Get fieldSid with
                | None -> None
                | Some fieldInfo ->
                    let typeStr = PositionIndex.formatTypeWithTable symbolTable fieldInfo.typ
                    Some(CompletionItem(fieldInfo.name, kind = CompletionItemKind.Field, detail = typeStr)))
        | _ -> []

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
    /// カーソル直前がピリオドの場合はドット補完を行い、レシーバーの型メンバーを返す。
    /// それ以外はモジュールスコープ内の全シンボルを候補として返す（通常補完）。
    member this.GetCompletions(uri: string, line: int, character: int) : CompletionList =
        match this.TryGetIndex uri with
        | None -> CompletionList(false, [])
        | Some(index, symbolTable) ->
            // バッファテキストを取得し、ドット補完コンテキストを検出する。
            let dotContext =
                match tryNormalizeUri uri with
                | None -> None
                | Some key ->
                    match buffers.TryGetValue key with
                    | false, _ -> None
                    | true, text -> detectDotContext text line character
            match dotContext with
            | Some(_, identLine, identCol) ->
                // ドット補完：ピリオド前の識別子の型を特定し、その型のメンバーを返す。
                match PositionIndex.tryFindSymbolAt index identLine identCol with
                | None -> CompletionList(false, [])
                | Some sid ->
                    match symbolTable.Get sid with
                    | None -> CompletionList(false, [])
                    | Some symInfo ->
                        let items = getMembersOfType symbolTable index symInfo.typ
                        CompletionList(false, items)
            | None ->
                // 通常補完：スコープ内の全シンボルを返す。
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
                            // detail: VS Code の補完 UI に型シグネチャとして表示されるテキスト。
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
