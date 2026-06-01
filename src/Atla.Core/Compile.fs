namespace Atla.Compiler

open Atla.Core.Data
open Atla.Core.Syntax
open Atla.Core.Syntax.Data
open Atla.Core.Semantics
open Atla.Core.Semantics.Data
open Atla.Core.Lowering
open System.Collections.Generic
open System.IO

module Compiler =
    type ResolvedDependency =
        { name: string
          version: string
          source: string
          compileReferencePaths: string list
          runtimeLoadPaths: string list
          /// ネイティブランタイム DLL のパスリスト（runtimes/<rid>/native/ 配下のファイル）。
          nativeRuntimePaths: string list }

    type ModuleSource =
        { moduleName: string
          source: string
          /// ソースファイルの表示用パス（エラーメッセージに使用）。
          /// 省略時はモジュール名を代わりに使用する。
          filePath: string option }

    type CompileModulesRequest =
        { asmName: string
          modules: ModuleSource list
          entryModuleName: string
          outDir: string
          dependencies: ResolvedDependency list }

    type CompileResult =
        { succeeded: bool
          diagnostics: Diagnostic list
          /// 意味解析が成功した場合に得られる HIR アセンブリ。
          /// パイプラインの後続フェーズが失敗した場合でも返される（IntelliSense 用）。
          hir: Hir.Assembly option
          /// パッケージング用に保持する、モジュールごとの HIR 一覧。
          hirModules: Hir.Module list option
          /// 意味解析が成功した場合に得られるシンボルテーブル。
          /// パイプラインの後続フェーズが失敗した場合でも返される（IntelliSense 用）。
          symbolTable: SymbolTable option
          /// パッケージング用に保持する、モジュール名→AST の対応表。
          moduleAsts: Map<string, Ast.Module> option }

        member this.hasErrors = this.diagnostics |> List.exists (fun diagnostic -> diagnostic.isError)

    /// コンパイル失敗の結果を構築する。hir・symbolTable はオプションで提供できる。
    let private failed
        (diagnostics: Diagnostic list)
        (hir: Hir.Assembly option)
        (hirModules: Hir.Module list option)
        (symbolTable: SymbolTable option)
        (moduleAsts: Map<string, Ast.Module> option)
        : CompileResult =
        { succeeded   = false
          diagnostics = diagnostics
          hir         = hir
          hirModules  = hirModules
          symbolTable = symbolTable
          moduleAsts  = moduleAsts }

    /// コンパイル成功の結果を構築する。
    let private succeeded
        (diagnostics: Diagnostic list)
        (hir: Hir.Assembly option)
        (hirModules: Hir.Module list option)
        (symbolTable: SymbolTable option)
        (moduleAsts: Map<string, Ast.Module> option)
        : CompileResult =
        { succeeded   = true
          diagnostics = diagnostics
          hir         = hir
          hirModules  = hirModules
          symbolTable = symbolTable
          moduleAsts  = moduleAsts }

    /// 1 モジュール分のソース文字列を AST へ解析する。
    let private parseModuleSource (moduleName: string) (filePath: string option) (source: string) : Result<Ast.Module, Diagnostic list> =
        let sourceLabel = filePath |> Option.defaultValue moduleName
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (moduleAst, _) -> Ok moduleAst
            | Failure (reason, span) -> Result.Error [ Diagnostic.Error($"{sourceLabel}: {reason}", span) ]
        | Failure (reason, span) ->
            Result.Error [ Diagnostic.Error($"{sourceLabel}: {reason}", span) ]

    /// import 依存グラフを DFS で辿り、エントリモジュールから必要なモジュールのトポロジカル順序を返す。
    let private topoSortModulesFromEntry
        (entryModuleName: string)
        (moduleAsts: Map<string, Ast.Module>)
        : Result<string list, Diagnostic list> =
        let moduleNames = moduleAsts |> Map.keys |> Set.ofSeq

        let getImportedModuleNames (moduleAst: Ast.Module) : string list =
            moduleAst.decls
            |> List.choose (fun decl ->
                match decl with
                | :? Ast.Decl.Import as importDecl ->
                    let importName = String.concat "." importDecl.path
                    if moduleNames.Contains(importName) then
                        // `import foo'bar` が Atla モジュールとして存在する場合はそのまま依存に追加する。
                        Some importName
                    elif importDecl.path.Length >= 2 then
                        // `import Foo'Bar` が型 import として解決される場合でも、
                        // `Foo` モジュールを先行解析しないと exported method 参照が欠落するため依存へ加える。
                        let parentModuleName = importDecl.path |> List.take (importDecl.path.Length - 1) |> String.concat "."
                        if moduleNames.Contains(parentModuleName) then Some parentModuleName else None
                    else
                        None
                | _ -> None)

        let rec visit (stack: string list) (visited: Set<string>) (ordered: string list) (moduleName: string)
            : Result<Set<string> * string list, Diagnostic list> =
            if stack |> List.contains moduleName then
                let cyclePath = (moduleName :: stack) |> List.rev |> String.concat " -> "
                Result.Error [ Diagnostic.Error($"import cycle detected: {cyclePath}", Span.Empty) ]
            elif visited.Contains moduleName then
                Ok (visited, ordered)
            else
                match moduleAsts.TryFind moduleName with
                | None -> Result.Error [ Diagnostic.Error($"entry module '{moduleName}' was not found", Span.Empty) ]
                | Some moduleAst ->
                    let imports = getImportedModuleNames moduleAst
                    let folder state imported =
                        match state with
                        | Result.Error diagnostics -> Result.Error diagnostics
                        | Ok (visitedAcc, orderedAcc) -> visit (moduleName :: stack) visitedAcc orderedAcc imported
                    match imports |> List.fold folder (Ok (visited, ordered)) with
                    | Result.Error diagnostics -> Result.Error diagnostics
                    | Ok (visitedAfterDeps, orderedAfterDeps) ->
                        Ok (visitedAfterDeps.Add moduleName, orderedAfterDeps @ [ moduleName ])

        match visit [] Set.empty [] entryModuleName with
        | Result.Ok (_, ordered) -> Ok ordered
        | Result.Error diagnostics -> Result.Error diagnostics

    /// HIR モジュールから公開シンボル（トップレベル関数・メソッド・データ型）を抽出する。
    /// データ型は "type:{TypeName}" というキーでエクスポートし、他モジュールが import 時に
    /// 元の typeSid を再利用できるようにする。
    let private collectModuleExports
        (symbolTable: SymbolTable)
        (hirModule: Hir.Module)
        : Map<string, AnalyzeEnv.ModuleExport> =
        let valueExports =
            hirModule.scope.vars
            |> Seq.map (fun kv -> kv.Key, kv.Value)
            |> Seq.choose (fun (name, sid) ->
                symbolTable.Get(sid)
                |> Option.map (fun symInfo ->
                    name,
                    ({ symbolId = sid
                       typ = symInfo.typ }: AnalyzeEnv.ModuleExport)))
            |> Seq.toList

        let methodExports =
            hirModule.methods
            |> List.choose (fun methodInfo ->
                symbolTable.Get(methodInfo.sym)
                |> Option.map (fun symInfo ->
                    symInfo.name,
                    ({ symbolId = methodInfo.sym
                       typ = symInfo.typ }: AnalyzeEnv.ModuleExport)))

        // インスタンス impl メソッドは hirType.methods へ移動したため、
        // 非 interface 型のメソッドもエクスポートに含める。
        // これにより、クロスモジュールのメソッドインポートが正しく解決される。
        let typeInstanceMethodExports =
            hirModule.types
            |> List.filter (fun hirType -> not hirType.isInterface)
            |> List.collect (fun hirType ->
                hirType.methods
                |> List.choose (fun methodInfo ->
                    symbolTable.Get(methodInfo.sym)
                    |> Option.map (fun symInfo ->
                        symInfo.name,
                        ({ symbolId = methodInfo.sym
                           typ = symInfo.typ }: AnalyzeEnv.ModuleExport))))

        // データ型の typeSid を "type:{TypeName}" キーでエクスポートする。
        // import sub'Person のような型 import 時に元の typeSid を再利用するために使用する。
        let typeExports =
            hirModule.types
            |> List.choose (fun hirType ->
                symbolTable.Get(hirType.sym)
                |> Option.map (fun symInfo ->
                    $"type:{symInfo.name}",
                    ({ symbolId = hirType.sym
                       typ = TypeId.Name hirType.sym }: AnalyzeEnv.ModuleExport)))

        // データ型のフィールド SID を "field:{TypeName}.{FieldName}" キーでエクスポートする。
        // import sub'Person のような型 import 時に元のフィールド SID を再利用するために使用する。
        let fieldExports =
            hirModule.types
            |> List.collect (fun hirType ->
                hirType.fields
                |> List.choose (fun field ->
                    symbolTable.Get(field.sym)
                    |> Option.map (fun fieldInfo ->
                        $"field:{fieldInfo.name}",
                        ({ symbolId = field.sym
                           typ = field.typ }: AnalyzeEnv.ModuleExport))))

        // `impl X as DotNetBase` で確定した .NET 基底型を "implBase:{TypeName}" キーでエクスポートする。
        // import 先モジュールで DataTypeDef.baseType を復元し、継承チェーンのメンバー解決を可能にする。
        let implBaseExports =
            hirModule.types
            |> List.choose (fun hirType ->
                match hirType.baseType with
                | None -> None
                | Some baseType ->
                    symbolTable.Get(hirType.sym)
                    |> Option.map (fun symInfo ->
                        $"implBase:{symInfo.name}",
                        ({ symbolId = hirType.sym
                           typ = baseType }: AnalyzeEnv.ModuleExport)))

        valueExports @ methodExports @ typeInstanceMethodExports @ typeExports @ fieldExports @ implBaseExports
        |> Map.ofList

    /// 複数 HIR モジュールを 1 つへ統合し、CIL 生成のランタイム制約（単一 dynamic module）に合わせる。
    let private mergeHirModules (entryModuleName: string) (hirModules: Hir.Module list) : Hir.Module =
        let mergedScope = Scope(None)

        hirModules
        |> List.collect (fun modul -> modul.scope.vars |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.toList)
        |> List.iter (fun (name, sid) ->
            if not (mergedScope.vars.ContainsKey(name)) then
                mergedScope.DeclareVar(name, sid))

        hirModules
        |> List.collect (fun modul -> modul.scope.types |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.toList)
        |> List.iter (fun (name, tid) ->
            if not (mergedScope.types.ContainsKey(name)) then
                mergedScope.DeclareType(name, tid))

        let mergedTypes = hirModules |> List.collect (fun modul -> modul.types)
        let mergedFields = hirModules |> List.collect (fun modul -> modul.fields)
        let mergedMethods = hirModules |> List.collect (fun modul -> modul.methods)
        Hir.Module(entryModuleName, mergedTypes, mergedFields, mergedMethods, mergedScope)

    /// 依存解決・意味解析・lowering を含む実コンパイルを実行する（複数モジュール入力対応）。
    let private compileModulesWithDependencyLoadPolicy
        (dependencyLoadPolicy: DependencyLoader.DependencyLoadPolicy)
        (request: CompileModulesRequest)
        : CompileResult =
        if List.isEmpty request.modules then
            failed [ Diagnostic.Error("No source modules were provided", Span.Empty) ] None None None None
        else
            try
                // 各モジュールのソースを AST へ解析する。
                // Std などの標準ライブラリは依存関係（atlalib）として供給され、
                // 埋め込みソースの自動注入は行わない。
                let moduleFilePaths =
                    request.modules
                    |> List.choose (fun m -> m.filePath |> Option.map (fun fp -> m.moduleName, fp))
                    |> Map.ofList

                let parsedModulesResult =
                    request.modules
                    |> List.fold
                        (fun state moduleSource ->
                            match state with
                            | Result.Error diagnostics -> Result.Error diagnostics
                            | Ok modules ->
                                match parseModuleSource moduleSource.moduleName moduleSource.filePath moduleSource.source with
                                | Ok moduleAst -> Ok(Map.add moduleSource.moduleName moduleAst modules)
                                | Result.Error diagnostics -> Result.Error diagnostics)
                        (Ok Map.empty)

                match parsedModulesResult with
                | Result.Error diagnostics ->
                    failed diagnostics None None None None
                | Ok moduleAsts ->
                    match topoSortModulesFromEntry request.entryModuleName moduleAsts with
                    | Result.Error diagnostics ->
                        failed diagnostics None None None (Some moduleAsts)
                    | Ok orderedModuleNames ->
                        let dependencyInputs =
                            request.dependencies
                            |> List.map (fun dependency -> dependency.name, dependency.runtimeLoadPaths)

                        match DependencyLoader.loadDependenciesWithPolicy dependencyLoadPolicy dependencyInputs with
                        | { succeeded = false; diagnostics = dependencyDiagnostics } ->
                            failed dependencyDiagnostics None None None (Some moduleAsts)
                        | { loadContext = dependencyLoadContext } ->
                            try
                                let symbolTable = SymbolTable()
                                let typeSubst = TypeSubst()
                                // typeMetaFactory を全モジュール間で共有し、メタ ID の衝突を防ぐ。
                                // 各モジュールごとに新しいファクトリを生成すると、先に解析したモジュールが
                                // typeSubst に書き込んだメタ束縛を後続モジュールが誤って参照してしまう。
                                let typeMetaFactory = TypeMetaFactory()
                                let sourceModuleNames = moduleAsts |> Map.keys |> Set.ofSeq
                                let sourceTypeFullNames =
                                    moduleAsts
                                    |> Map.toList
                                    |> List.collect (fun (moduleName, moduleAst) ->
                                        moduleAst.decls
                                        |> List.choose (fun decl ->
                                            match decl with
                                            | :? Ast.Decl.Data as dataDecl -> Some $"{moduleName}.{dataDecl.name}"                                            | :? Ast.Decl.Union as unionDecl -> Some $"{moduleName}.{unionDecl.name}"
                                            | _ -> None))
                                    |> Set.ofList
                                let availableTypeDecls =
                                    moduleAsts
                                    |> Map.toList
                                    |> List.collect (fun (moduleName, moduleAst) ->
                                        moduleAst.decls
                                        |> List.choose (fun decl ->
                                            match decl with
                                            | :? Ast.Decl.Data as dataDecl -> Some ($"{moduleName}.{dataDecl.name}", dataDecl :> Ast.Decl)                                            | :? Ast.Decl.Union as unionDecl -> Some ($"{moduleName}.{unionDecl.name}", unionDecl :> Ast.Decl)
                                            | _ -> None))
                                    |> Map.ofList
                                let availableDataTypeImplDecls =
                                    moduleAsts
                                    |> Map.toList
                                    |> List.collect (fun (moduleName, moduleAst) ->
                                        moduleAst.decls
                                        |> List.choose (fun decl ->
                                            match decl with
                                            | :? Ast.Decl.Impl as implDecl -> Some (moduleName, implDecl)
                                            | _ -> None))
                                    |> List.fold
                                        (fun (acc: Map<string, Ast.Decl.Impl list>) (moduleName, implDecl) ->
                                            let fullTypePath = $"{moduleName}.{implDecl.typeName}"
                                            match Map.tryFind fullTypePath acc with
                                            | Some impls -> Map.add fullTypePath (impls @ [ implDecl ]) acc
                                            | None -> Map.add fullTypePath [ implDecl ] acc)
                                        Map.empty
                                let dependencyIndex =
                                    request.dependencies
                                    |> List.map (fun dependency -> dependency.source)
                                    |> AtlaLib.loadDependencyIndex symbolTable

                                let analyzeFolder (hirModules, moduleExports, diagnostics) moduleName =
                                    let moduleAst = moduleAsts[moduleName]
                                    let analyzeResult =
                                        Analyze.analyzeModuleWithImports(
                                            symbolTable,
                                            typeSubst,
                                            typeMetaFactory,
                                            moduleName,
                                            moduleAst,
                                            sourceModuleNames,
                                            sourceTypeFullNames,
                                            dependencyIndex.moduleNames,
                                            dependencyIndex.typeFullNames,
                                            availableTypeDecls,
                                            availableDataTypeImplDecls,
                                            Map.fold (fun acc key value -> Map.add key value acc) dependencyIndex.moduleExports moduleExports,
                                            dependencyIndex.typeDefsByFullName
                                        )

                                    let sourceLabel = Map.tryFind moduleName moduleFilePaths |> Option.defaultValue moduleName
                                    let taggedDiagnostics = analyzeResult.diagnostics |> List.map (fun d -> d.WithSource(sourceLabel))

                                    match analyzeResult.value with
                                    | Some hirModule ->
                                        let ownExports = collectModuleExports symbolTable hirModule
                                        let allAvailable =
                                            Map.fold (fun acc k v -> Map.add k v acc) dependencyIndex.moduleExports moduleExports
                                        let reExportedTypes =
                                            moduleAst.decls
                                            |> List.choose (fun decl ->
                                                match decl with
                                                | :? Ast.Decl.Import as importDecl when importDecl.isPublic ->
                                                    let fullModuleName = String.concat "." importDecl.path
                                                    allAvailable |> Map.tryFind fullModuleName
                                                | _ -> None)
                                            |> List.collect (fun exports ->
                                                exports |> Map.toList |> List.filter (fun (key, _) -> key.StartsWith("type:")))
                                            |> Map.ofList
                                        let finalExports =
                                            Map.fold (fun acc k v -> Map.add k v acc) ownExports reExportedTypes
                                        hirModules @ [ hirModule ], Map.add moduleName finalExports moduleExports, diagnostics @ taggedDiagnostics
                                    | None ->
                                        hirModules, moduleExports, diagnostics @ taggedDiagnostics

                                let hirModules, _, allDiagnostics =
                                    orderedModuleNames
                                    |> List.fold analyzeFolder ([], dependencyIndex.moduleExports, dependencyIndex.diagnostics)

                                // 全ソースモジュールをマージして HIR アセンブリを構築する。
                                // Std などの標準ライブラリはソースではなく依存関係（atlalib）として
                                // 供給されるため、hirModules にはユーザーモジュールのみが含まれる。
                                let mergedModule = mergeHirModules request.entryModuleName hirModules
                                let hirAsm = Hir.Assembly(request.asmName, [ mergedModule ])

                                if allDiagnostics |> List.exists (fun d -> d.isError) then
                                    failed allDiagnostics (Some hirAsm) (Some hirModules) (Some symbolTable) (Some moduleAsts)
                                else
                                    match ClosureConversion.preprocessAssembly(symbolTable, hirAsm) with
                                    | { succeeded = false; diagnostics = closureDiagnostics } ->
                                        failed
                                            (allDiagnostics @ closureDiagnostics)
                                            (Some hirAsm)
                                            (Some hirModules)
                                            (Some symbolTable)
                                            (Some moduleAsts)
                                    | { value = Some closedAsm; diagnostics = closureDiagnostics } ->
                                        // PR-3a: `async fn` の本体 Task ラップと await 同期化を施す。
                                        // PR-3b で状態機械生成に置き換える予定。
                                        let rewrittenAsm = AsyncRewrite.rewriteAssembly symbolTable closedAsm
                                        match Layout.layoutAssembly(request.asmName, rewrittenAsm) with
                                        | { succeeded = false; diagnostics = layoutDiagnostics } ->
                                            failed
                                                (allDiagnostics @ closureDiagnostics @ layoutDiagnostics)
                                                (Some hirAsm)
                                                (Some hirModules)
                                                (Some symbolTable)
                                                (Some moduleAsts)
                                        | { value = Some mir; diagnostics = layoutDiagnostics } ->
                                            let outPath = Path.Join(request.outDir, sprintf "%s.dll" request.asmName)
                                            match Gen.genAssembly(mir, outPath, symbolTable) with
                                            | { succeeded = false; diagnostics = genDiagnostics } ->
                                                failed
                                                    (allDiagnostics @ closureDiagnostics @ layoutDiagnostics @ genDiagnostics)
                                                    (Some hirAsm)
                                                    (Some hirModules)
                                                    (Some symbolTable)
                                                    (Some moduleAsts)
                                            | { diagnostics = genDiagnostics } ->
                                                succeeded
                                                    (allDiagnostics @ closureDiagnostics @ layoutDiagnostics @ genDiagnostics)
                                                    (Some hirAsm)
                                                    (Some hirModules)
                                                    (Some symbolTable)
                                                    (Some moduleAsts)
                                        | _ ->
                                            failed
                                                (allDiagnostics @ closureDiagnostics @ [ Diagnostic.Error("Lowering failed with unknown state", Span.Empty) ])
                                                (Some hirAsm)
                                                (Some hirModules)
                                                (Some symbolTable)
                                                (Some moduleAsts)
                                    | _ ->
                                        failed
                                            (allDiagnostics @ [ Diagnostic.Error("Closure conversion failed with unknown state", Span.Empty) ])
                                            (Some hirAsm)
                                            (Some hirModules)
                                            (Some symbolTable)
                                            (Some moduleAsts)
                            finally
                                DependencyLoader.unloadDependencies dependencyLoadContext
            with ex ->
                failed [ Diagnostic.Error($"Compilation failed: [{ex.GetType().Name}] {ex.Message}\n{ex.StackTrace}", Span.Empty) ] None None None None

    /// 通常ビルド向けコンパイル（依存DLLは元パスを直接ロード）。
    let compileModules (request: CompileModulesRequest) : CompileResult =
        compileModulesWithDependencyLoadPolicy DependencyLoader.DependencyLoadPolicy.Direct request

    /// Language Server 向けコンパイル（依存DLLはローカルコピーを経由してロード）。
    let compileModulesForLanguageServer (request: CompileModulesRequest) : CompileResult =
        compileModulesWithDependencyLoadPolicy DependencyLoader.DependencyLoadPolicy.LocalCopyCache request
