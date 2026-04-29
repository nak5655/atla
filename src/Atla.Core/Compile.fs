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
          source: string }

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
          /// 意味解析が成功した場合に得られるシンボルテーブル。
          /// パイプラインの後続フェーズが失敗した場合でも返される（IntelliSense 用）。
          symbolTable: SymbolTable option }

        member this.hasErrors = this.diagnostics |> List.exists (fun diagnostic -> diagnostic.isError)

    /// コンパイル失敗の結果を構築する。hir・symbolTable はオプションで提供できる。
    let private failed
        (diagnostics: Diagnostic list)
        (hir: Hir.Assembly option)
        (symbolTable: SymbolTable option)
        : CompileResult =
        { succeeded   = false
          diagnostics = diagnostics
          hir         = hir
          symbolTable = symbolTable }

    /// コンパイル成功の結果を構築する。
    let private succeeded
        (diagnostics: Diagnostic list)
        (hir: Hir.Assembly option)
        (symbolTable: SymbolTable option)
        : CompileResult =
        { succeeded   = true
          diagnostics = diagnostics
          hir         = hir
          symbolTable = symbolTable }

    /// 1 モジュール分のソース文字列を AST へ解析する。
    let private parseModuleSource (moduleName: string) (source: string) : Result<Ast.Module, Diagnostic list> =
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Success (moduleAst, _) -> Ok moduleAst
            | Failure (reason, span) -> Result.Error [ Diagnostic.Error($"Parsing failed in module '{moduleName}': {reason}", span) ]
        | Failure (reason, span) ->
            Result.Error [ Diagnostic.Error($"Lexing failed in module '{moduleName}': {reason}", span) ]

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
                    if moduleNames.Contains(importName) then Some importName else None
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

    /// HIR モジュールから公開シンボル（現時点ではトップレベル関数）を抽出する。
    let private collectModuleExports
        (symbolTable: SymbolTable)
        (hirModule: Hir.Module)
        : Map<string, Analyze.ModuleExport> =
        let valueExports =
            hirModule.scope.vars
            |> Seq.map (fun kv -> kv.Key, kv.Value)
            |> Seq.choose (fun (name, sid) ->
                symbolTable.Get(sid)
                |> Option.map (fun symInfo ->
                    name,
                    ({ symbolId = sid
                       typ = symInfo.typ }: Analyze.ModuleExport)))
            |> Seq.toList

        let methodExports =
            hirModule.methods
            |> List.choose (fun methodInfo ->
                symbolTable.Get(methodInfo.sym)
                |> Option.map (fun symInfo ->
                    symInfo.name,
                    ({ symbolId = methodInfo.sym
                       typ = symInfo.typ }: Analyze.ModuleExport)))

        valueExports @ methodExports
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
    let compileModules (request: CompileModulesRequest) : CompileResult =
        if List.isEmpty request.modules then
            failed [ Diagnostic.Error("No source modules were provided", Span.Empty) ] None None
        else
            try
                let parsedModulesResult =
                    request.modules
                    |> List.fold
                        (fun state moduleSource ->
                            match state with
                            | Result.Error diagnostics -> Result.Error diagnostics
                            | Ok modules ->
                                match parseModuleSource moduleSource.moduleName moduleSource.source with
                                | Ok moduleAst -> Ok(Map.add moduleSource.moduleName moduleAst modules)
                                | Result.Error diagnostics -> Result.Error diagnostics)
                        (Ok Map.empty)

                match parsedModulesResult with
                | Result.Error diagnostics ->
                    failed diagnostics None None
                | Ok moduleAsts ->
                    match topoSortModulesFromEntry request.entryModuleName moduleAsts with
                    | Result.Error diagnostics ->
                        failed diagnostics None None
                    | Ok orderedModuleNames ->
                        let dependencyInputs =
                            request.dependencies
                            |> List.map (fun dependency -> dependency.name, dependency.runtimeLoadPaths)

                        match DependencyLoader.loadDependencies dependencyInputs with
                        | { succeeded = false; diagnostics = dependencyDiagnostics } ->
                            failed dependencyDiagnostics None None
                        | { loadContext = dependencyLoadContext } ->
                            try
                                let symbolTable = SymbolTable()
                                let typeSubst = TypeSubst()
                                let availableModuleNames = moduleAsts |> Map.keys |> Set.ofSeq
                                let availableTypeFullNames =
                                    moduleAsts
                                    |> Map.toList
                                    |> List.collect (fun (moduleName, moduleAst) ->
                                        moduleAst.decls
                                        |> List.choose (fun decl ->
                                            match decl with
                                            | :? Ast.Decl.Data as dataDecl -> Some $"{moduleName}.{dataDecl.name}"
                                            | _ -> None))
                                    |> Set.ofList
                                let availableDataTypeDecls =
                                    moduleAsts
                                    |> Map.toList
                                    |> List.collect (fun (moduleName, moduleAst) ->
                                        moduleAst.decls
                                        |> List.choose (fun decl ->
                                            match decl with
                                            | :? Ast.Decl.Data as dataDecl -> Some ($"{moduleName}.{dataDecl.name}", dataDecl)
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

                                let analyzeFolder (hirModules, moduleExports, diagnostics) moduleName =
                                    let moduleAst = moduleAsts[moduleName]
                                    let analyzeResult =
                                        Analyze.analyzeModuleWithImports(
                                            symbolTable,
                                            typeSubst,
                                            moduleName,
                                            moduleAst,
                                            availableModuleNames,
                                            availableTypeFullNames,
                                            availableDataTypeDecls,
                                            availableDataTypeImplDecls,
                                            moduleExports
                                        )

                                    match analyzeResult.value with
                                    | Some hirModule ->
                                        let exports = collectModuleExports symbolTable hirModule
                                        hirModules @ [ hirModule ], Map.add moduleName exports moduleExports, diagnostics @ analyzeResult.diagnostics
                                    | None ->
                                        hirModules, moduleExports, diagnostics @ analyzeResult.diagnostics

                                let hirModules, _, allDiagnostics =
                                    orderedModuleNames
                                    |> List.fold analyzeFolder ([], Map.empty, [])

                                let mergedModule = mergeHirModules request.entryModuleName hirModules
                                let hirAsm = Hir.Assembly(request.asmName, [ mergedModule ])

                                if allDiagnostics |> List.exists (fun d -> d.isError) then
                                    failed allDiagnostics (Some hirAsm) (Some symbolTable)
                                else
                                    match ClosureConversion.preprocessAssembly(symbolTable, hirAsm) with
                                    | { succeeded = false; diagnostics = closureDiagnostics } ->
                                        failed (allDiagnostics @ closureDiagnostics) (Some hirAsm) (Some symbolTable)
                                    | { value = Some closedAsm; diagnostics = closureDiagnostics } ->
                                        match Layout.layoutAssembly(request.asmName, closedAsm) with
                                        | { succeeded = false; diagnostics = layoutDiagnostics } ->
                                            failed (allDiagnostics @ closureDiagnostics @ layoutDiagnostics) (Some hirAsm) (Some symbolTable)
                                        | { value = Some mir; diagnostics = layoutDiagnostics } ->
                                            let outPath = Path.Join(request.outDir, sprintf "%s.dll" request.asmName)
                                            match Gen.genAssembly(mir, outPath, symbolTable) with
                                            | { succeeded = false; diagnostics = genDiagnostics } ->
                                                failed (allDiagnostics @ closureDiagnostics @ layoutDiagnostics @ genDiagnostics) (Some hirAsm) (Some symbolTable)
                                            | { diagnostics = genDiagnostics } ->
                                                succeeded (allDiagnostics @ closureDiagnostics @ layoutDiagnostics @ genDiagnostics) (Some hirAsm) (Some symbolTable)
                                        | _ ->
                                            failed (allDiagnostics @ closureDiagnostics @ [ Diagnostic.Error("Lowering failed with unknown state", Span.Empty) ]) (Some hirAsm) (Some symbolTable)
                                    | _ ->
                                        failed (allDiagnostics @ [ Diagnostic.Error("Closure conversion failed with unknown state", Span.Empty) ]) (Some hirAsm) (Some symbolTable)
                            finally
                                DependencyLoader.unloadDependencies dependencyLoadContext
            with ex ->
                failed [ Diagnostic.Error($"Compilation failed: {ex.Message}", Span.Empty) ] None None
