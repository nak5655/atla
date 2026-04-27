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

/// 補完候補のプレフィックス抽出に使う識別子文字判定。
let private isIdentifierChar (ch: char) : bool =
    Char.IsLetterOrDigit(ch) || ch = '_'

/// 行アクセス用途で改行コード差分（LF/CRLF/CR）を吸収する。
let private normalizeForLineAccess (text: string) : string =
    if isNull text then "" else text.Replace("\r\n", "\n").Replace("\r", "\n")

/// 補完計算時の文脈（通常補完 / メンバー補完）を表す。
type private CompletionContext =
    | ModuleScope of prefix: string
    | MemberAccess of receiverName: string * receiverLine: int * receiverProbeColumn: int * memberPrefix: string

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
    /// キャッシュ: 正規化 URI → (PositionIndex, SymbolTable, Typed HIR Assembly)。
    /// コンパイルが意味解析フェーズを通過した場合に格納され、IntelliSense クエリに使用する。
    let hirCache = Dictionary<string, PositionIndex.PositionIndex * SymbolTable * Hir.Assembly>()
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
                        hirCache[normalizedUri] <- (index, symTable, hirAsm)
                    | _ ->
                        // 入力途中の構文エラーでも補完を維持するため、直前の成功キャッシュを保持する。
                        ()

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

        // 補完トリガーは `.` を広告せず、メンバーアクセス記号 `'` を既定とする。
        let capabilities =
            ServerCapabilities(
                false,
                TextDocumentSyncOptions(true, TextDocumentSyncKind.Full),
                SemanticTokensOptions(
                    SemanticTokensLegend(tokenTypes |> Array.toList, []),
                    false,
                    true
                ),
                CompletionOptions([ "'" ]),
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

    /// 指定した URI の PositionIndex / シンボルテーブル / HIR をキャッシュから取得する。
    member private _.TryGetCachedSemanticState
        (uri: string)
        : (PositionIndex.PositionIndex * SymbolTable * Hir.Assembly) option =
        match tryNormalizeUri uri with
        | None -> None
        | Some key ->
            match hirCache.TryGetValue key with
            | true, entry -> Some entry
            | false, _ -> None

    /// 指定位置の補完文脈（通常補完 / メンバー補完）を抽出する。
    member private _.TryGetCompletionContext(uri: string, line: int, character: int) : CompletionContext option =
        match tryNormalizeUri uri with
        | None -> None
        | Some key ->
            match buffers.TryGetValue key with
            | false, _ -> None
            | true, text ->
                let lines = normalizeForLineAccess text |> fun x -> x.Split('\n')

                if line < 0 || line >= lines.Length then
                    Some(ModuleScope "")
                else
                    let lineText = lines.[line]
                    let cursor = min (max character 0) lineText.Length
                    let mutable start = cursor

                    while start > 0 && isIdentifierChar lineText.[start - 1] do
                        start <- start - 1

                    let prefix = lineText.Substring(start, cursor - start)
                    let quoteIndex = start - 1

                    if quoteIndex >= 0 && lineText.[quoteIndex] = '\'' then
                        let mutable receiverStart = quoteIndex
                        while receiverStart > 0 && isIdentifierChar lineText.[receiverStart - 1] do
                            receiverStart <- receiverStart - 1

                        if receiverStart < quoteIndex then
                            // receiverProbeColumn は receiver 識別子内部の列（末尾文字）を使う。
                            let receiverName = lineText.Substring(receiverStart, quoteIndex - receiverStart)
                            Some(MemberAccess(receiverName, line, quoteIndex - 1, prefix))
                        else
                            Some(ModuleScope prefix)
                    else
                        Some(ModuleScope prefix)

    /// 名前と型情報から補完候補 1 件を作成する。
    member private _.BuildCompletionItem
        (name: string, typ: TypeId, kind: CompletionItemKind option, symbolTable: SymbolTable)
        : CompletionItem =
        let detail = PositionIndex.formatTypeWithTable symbolTable typ
        CompletionItem(name, ?kind = kind, detail = detail)

    /// データ型メンバー候補を収集する。
    member private this.GetDataTypeMemberCompletions
        (typeSid: SymbolId, symbolTable: SymbolTable, hirAsm: Hir.Assembly, prefix: string)
        : CompletionItem list =
        let tryFindDataType () =
            hirAsm.modules
            |> List.collect (fun modul -> modul.types)
            |> List.tryFind (fun typ -> typ.sym = typeSid)

        match tryFindDataType () with
        | None -> []
        | Some dataType ->
            let fieldItems =
                dataType.fields
                |> List.choose (fun field ->
                    match symbolTable.Get field.sym with
                    | None -> None
                    | Some info ->
                        if prefix.Length > 0 && not (info.name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)) then
                            None
                        else
                            Some(this.BuildCompletionItem(info.name, info.typ, Some CompletionItemKind.Field, symbolTable)))

            let methodItems =
                dataType.methods
                |> List.choose (fun methodInfo ->
                    match symbolTable.Get methodInfo.sym with
                    | None -> None
                    | Some info ->
                        if prefix.Length > 0 && not (info.name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)) then
                            None
                        else
                            Some(this.BuildCompletionItem(info.name, info.typ, Some CompletionItemKind.Method, symbolTable)))

            fieldItems @ methodItems

    /// .NET ネイティブ型メンバー候補を収集する。
    member private this.GetNativeTypeMemberCompletions
        (systemType: System.Type, symbolTable: SymbolTable, prefix: string)
        : CompletionItem list =
        let bindingFlags = BindingFlags.Public ||| BindingFlags.Instance

        systemType.GetMembers(bindingFlags)
        |> Seq.choose (fun memberInfo ->
            let memberName = memberInfo.Name
            if prefix.Length > 0 && not (memberName.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)) then
                None
            else
                match memberInfo with
                | :? MethodInfo as methodInfo ->
                    let args =
                        methodInfo.GetParameters()
                        |> Seq.map (fun p -> TypeId.fromSystemType p.ParameterType)
                        |> Seq.toList
                    let ret = TypeId.fromSystemType methodInfo.ReturnType
                    let tid = TypeId.Fn(args, ret)
                    Some(this.BuildCompletionItem(memberName, tid, Some CompletionItemKind.Method, symbolTable))
                | :? PropertyInfo as propertyInfo ->
                    let tid = TypeId.fromSystemType propertyInfo.PropertyType
                    Some(this.BuildCompletionItem(memberName, tid, Some CompletionItemKind.Field, symbolTable))
                | :? FieldInfo as fieldInfo ->
                    let tid = TypeId.fromSystemType fieldInfo.FieldType
                    Some(this.BuildCompletionItem(memberName, tid, Some CompletionItemKind.Field, symbolTable))
                | _ -> None)
        |> Seq.distinctBy (fun item -> item.label)
        |> Seq.toList

    /// シンボルテーブルが利用できない場合の簡易ネイティブメンバー補完候補を収集する。
    member private _.GetNativeTypeMemberCompletionsWithoutSymbols
        (systemType: System.Type, prefix: string)
        : CompletionItem list =
        let bindingFlags = BindingFlags.Public ||| BindingFlags.Instance

        systemType.GetMembers(bindingFlags)
        |> Seq.choose (fun memberInfo ->
            let memberName = memberInfo.Name
            if prefix.Length > 0 && not (memberName.StartsWith(prefix, StringComparison.OrdinalIgnoreCase)) then
                None
            else
                match memberInfo with
                | :? MethodInfo -> Some(CompletionItem(memberName, kind = CompletionItemKind.Method, detail = "native member"))
                | :? PropertyInfo
                | :? FieldInfo -> Some(CompletionItem(memberName, kind = CompletionItemKind.Field, detail = "native member"))
                | _ -> None)
        |> Seq.distinctBy (fun item -> item.label)
        |> Seq.toList

    /// 補完候補リストを返す。
    /// モジュールスコープ内の全シンボルを候補として提供する。
    /// メンバーアクセス文脈（`receiver'prefix`）では receiver 型に応じたメンバー候補を返す。
    member this.GetCompletions(uri: string, line: int, character: int) : CompletionList =
        let completionContext = this.TryGetCompletionContext(uri, line, character)
        match this.TryGetCachedSemanticState uri with
        | None ->
            match completionContext with
            | Some(MemberAccess(_, _, _, memberPrefix)) ->
                // 直近の成功キャッシュがない場合でも `receiver'` 入力中は最低限 Object メンバーを提示する。
                let fallbackItems = this.GetNativeTypeMemberCompletionsWithoutSymbols(typeof<obj>, memberPrefix)
                CompletionList(false, fallbackItems)
            | Some(ModuleScope _)
            | None ->
                CompletionList(false, [])
        | Some(index, symbolTable, hirAsm) ->
            match completionContext with
            | Some(MemberAccess(receiverName, receiverLine, receiverProbeColumn, memberPrefix)) ->
                let memberItems =
                    let receiverSidOpt =
                        match PositionIndex.tryFindSymbolAt index receiverLine receiverProbeColumn with
                        | Some sid -> Some sid
                        | None ->
                            // 入力途中で位置インデックスが追随できない場合は名前でフォールバック解決する。
                            symbolTable.Entries()
                            |> List.tryPick (fun (sid, info) ->
                                if String.Equals(info.name, receiverName, StringComparison.Ordinal) then Some sid else None)

                    match receiverSidOpt with
                    | None -> this.GetNativeTypeMemberCompletions(typeof<obj>, symbolTable, memberPrefix)
                    | Some receiverSid ->
                        match symbolTable.Get receiverSid with
                        | None -> this.GetNativeTypeMemberCompletions(typeof<obj>, symbolTable, memberPrefix)
                        | Some receiverInfo ->
                            match receiverInfo.typ with
                            | TypeId.Name typeSid ->
                                // data 型を優先し、見つからなければ外部型（SystemTypeRef）として扱う。
                                let dataItems = this.GetDataTypeMemberCompletions(typeSid, symbolTable, hirAsm, memberPrefix)
                                if not dataItems.IsEmpty then
                                    dataItems
                                else
                                    match symbolTable.Get typeSid with
                                    | Some typeInfo ->
                                        match typeInfo.kind with
                                        | SymbolKind.External(ExternalBinding.SystemTypeRef systemType) ->
                                            this.GetNativeTypeMemberCompletions(systemType, symbolTable, memberPrefix)
                                        | _ -> []
                                    | None -> []
                            | TypeId.Native systemType ->
                                this.GetNativeTypeMemberCompletions(systemType, symbolTable, memberPrefix)
                            | primitiveOrFnType ->
                                match TypeId.tryToRuntimeSystemType primitiveOrFnType with
                                | Some systemType ->
                                    this.GetNativeTypeMemberCompletions(systemType, symbolTable, memberPrefix)
                                | None -> this.GetNativeTypeMemberCompletions(typeof<obj>, symbolTable, memberPrefix)

                CompletionList(false, memberItems)
            | Some(ModuleScope _)
            | None ->
                let normalizedPrefix =
                    match completionContext with
                    | Some(ModuleScope p) -> p
                    | _ -> ""

                let visibleVars = index.moduleScope.allVisibleVars()
                let items =
                    visibleVars
                    |> List.choose (fun (name, sid) ->
                        match symbolTable.Get sid with
                        | None -> None
                        | Some symInfo ->
                            if normalizedPrefix.Length > 0 && not (name.StartsWith(normalizedPrefix, StringComparison.OrdinalIgnoreCase)) then
                                None
                            else
                                let kind =
                                    match symInfo.kind with
                                    | SymbolKind.BuiltinOperator _ -> Some CompletionItemKind.Function
                                    | SymbolKind.Local _ -> Some CompletionItemKind.Variable
                                    | SymbolKind.Arg _ -> Some CompletionItemKind.Variable
                                    | SymbolKind.External(ExternalBinding.NativeMethodGroup _) -> Some CompletionItemKind.Method
                                    | SymbolKind.External(ExternalBinding.ConstructorGroup _) -> Some CompletionItemKind.Class
                                    | SymbolKind.External(ExternalBinding.SystemTypeRef _) -> Some CompletionItemKind.Class
                                Some(this.BuildCompletionItem(name, symInfo.typ, kind, symbolTable)))

                CompletionList(false, items)

    /// カーソル位置にある識別子のホバー情報を返す。
    member this.GetHover(uri: string, line: int, character: int) : Hover option =
        match this.TryGetCachedSemanticState uri with
        | None -> None
        | Some(index, symbolTable, _) ->
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
        match this.TryGetCachedSemanticState uri with
        | None -> None
        | Some(index, _, _) ->
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
