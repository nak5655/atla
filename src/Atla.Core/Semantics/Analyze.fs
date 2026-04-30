namespace Atla.Core.Semantics

open System.Collections.Generic
open Atla.Core.Syntax.Data
open Atla.Core.Semantics.Data
open Atla.Core.Semantics.Data.AnalyzeEnv

module Analyze =

    let analyzeModuleWithImports
        (
            symbolTable: SymbolTable,
            typeSubst: TypeSubst,
            moduleName: string,
            moduleAst: Ast.Module,
            availableModuleNames: Set<string>,
            availableTypeFullNames: Set<string>,
            availableDataTypeDecls: Map<string, Ast.Decl.Data>,
            availableDataTypeImplDecls: Map<string, Ast.Decl.Impl list>,
            importedModuleExports: Map<string, Map<string, ModuleExport>>
        )
        : PhaseResult<Hir.Module> =
        match Resolve.resolveModuleWithImports (symbolTable, moduleName, moduleAst, availableModuleNames, availableTypeFullNames) with
        | { succeeded = false; diagnostics = diagnostics } -> PhaseResult.failed diagnostics
        | { value = Some resolvedModule } ->
            let moduleExportView =
                resolvedModule.importedModules
                |> Map.toList
                |> List.choose (fun (alias, moduleName) ->
                    importedModuleExports
                    |> Map.tryFind moduleName
                    |> Option.map (fun exports -> alias, exports))
                |> Map.ofList

            let bootstrapNameEnv = NameEnv(symbolTable, resolvedModule.moduleScope, Map.empty, moduleExportView)
            let typeEnv = TypeEnv(typeSubst, TypeMetaFactory())

            let fields = List<Hir.Field>()
            let methods = List<Hir.Method>()
            let types = List<Hir.Type>()

            // data 宣言を型定義へ正規化し、後続の式解析で参照するメタデータを構築する。
            let dataTypeDefs =
                resolvedModule.dataDecls
                |> List.fold
                    (fun defs resolvedDataDecl ->
                        let resolvedFields =
                            resolvedDataDecl.decl.items
                            |> List.choose (fun item ->
                                match item with
                                | :? Ast.DataItem.Field as fieldItem ->
                                    let fieldType = bootstrapNameEnv.resolveTypeExpr fieldItem.typeExpr
                                    let fieldSid = symbolTable.NextId()
                                    symbolTable.Add(fieldSid, { name = $"{resolvedDataDecl.decl.name}.{fieldItem.name}"; typ = fieldType; kind = SymbolKind.Local() })
                                    Some { name = fieldItem.name; sid = fieldSid; typ = fieldType; span = fieldItem.span }
                                | _ -> None)

                        let hirFields =
                            resolvedFields
                            |> List.map (fun fieldDef ->
                                Hir.Field(fieldDef.sid, fieldDef.typ, Hir.Expr.Unit(fieldDef.span), fieldDef.span))
                        types.Add(Hir.Type(resolvedDataDecl.typeSid, None, hirFields, []))
                        Map.add
                            resolvedDataDecl.decl.name
                            { typeSid = resolvedDataDecl.typeSid
                              baseTypeSid = None
                              delegatedByFieldName = None
                              fields = resolvedFields
                              methods = Map.empty }
                            defs)
                    Map.empty

            let importedDataTypeDefs, importedTypeDiagnostics =
                resolvedModule.importedTypeAliases
                |> Map.toList
                |> List.fold
                    (fun (defs, diagnostics) (aliasName, fullTypePath) ->
                        match availableDataTypeDecls.TryFind(fullTypePath) with
                        | None ->
                            defs, diagnostics @ [ Diagnostic.Error(sprintf "Imported type '%s' was not found" fullTypePath, Atla.Core.Data.Span.Empty) ]
                        | Some dataDecl ->
                            // 元モジュールが "type:{TypeName}" としてエクスポートした typeSid を再利用する。
                            // 再利用することでメソッドの引数型（元 typeSid）とレシーバー型（インポート typeSid）が
                            // 一致し、クロスモジュールのメソッド呼び出しが正しく型検査される。
                            let lastDotForType = fullTypePath.LastIndexOf('.')
                            let typeNameForLookup = if lastDotForType > 0 then fullTypePath.Substring(lastDotForType + 1) else fullTypePath
                            let sourceModuleName = if lastDotForType > 0 then fullTypePath.Substring(0, lastDotForType) else ""
                            let sourceModuleExports = importedModuleExports |> Map.tryFind sourceModuleName |> Option.defaultValue Map.empty
                            let typeSid, isReusingExistingType =
                                match sourceModuleExports |> Map.tryFind $"type:{typeNameForLookup}" with
                                | Some exportInfo ->
                                    // 元 typeSid を再利用する。シンボルテーブルには既に登録済みのため追加不要。
                                    exportInfo.symbolId, true
                                | None ->
                                    // フォールバック: 元モジュールのエクスポートが見つからない場合は新規割り当て。
                                    let newSid = symbolTable.NextId()
                                    symbolTable.Add(newSid, { name = aliasName; typ = TypeId.Name newSid; kind = SymbolKind.Local() })
                                    newSid, false
                            resolvedModule.moduleScope.DeclareType(aliasName, TypeId.Name typeSid)

                            let resolvedFields =
                                dataDecl.items
                                |> List.choose (fun item ->
                                    match item with
                                    | :? Ast.DataItem.Field as fieldItem ->
                                        let fieldType = bootstrapNameEnv.resolveTypeExpr fieldItem.typeExpr
                                        // 元モジュールが "field:{TypeName}.{FieldName}" としてエクスポートしたフィールド SID を
                                        // 再利用する。再利用することで DataConstructor や DataField アクセスが
                                        // CIL 生成フェーズの fieldBuilders と一致する。
                                        let fieldSid =
                                            match sourceModuleExports |> Map.tryFind $"field:{typeNameForLookup}.{fieldItem.name}" with
                                            | Some exportInfo ->
                                                exportInfo.symbolId
                                            | None ->
                                                let newSid = symbolTable.NextId()
                                                symbolTable.Add(newSid, { name = $"{aliasName}.{fieldItem.name}"; typ = fieldType; kind = SymbolKind.Local() })
                                                newSid
                                        Some { name = fieldItem.name; sid = fieldSid; typ = fieldType; span = fieldItem.span }
                                    | _ -> None)

                            let importedDef =
                                { typeSid = typeSid
                                  baseTypeSid = None
                                  delegatedByFieldName = None
                                  fields = resolvedFields
                                  methods = Map.empty }
                            // 元 typeSid を再利用している場合は HIR 型ノードを追加しない。
                            // 元モジュールの HIR に既に Hir.Type が存在するため、マージ後に重複を防ぐ。
                            if not isReusingExistingType then
                                let hirFields =
                                    resolvedFields
                                    |> List.map (fun fieldDef ->
                                        Hir.Field(fieldDef.sid, fieldDef.typ, Hir.Expr.Unit(fieldDef.span), fieldDef.span))
                                types.Add(Hir.Type(typeSid, None, hirFields, []))
                            let importedImplMethodMap, importedImplDiagnostics =
                                let lastDot = fullTypePath.LastIndexOf('.')
                                if lastDot <= 0 then
                                    Map.empty, []
                                else
                                    let moduleName = fullTypePath.Substring(0, lastDot)
                                    let typeName = fullTypePath.Substring(lastDot + 1)
                                    let moduleExports = importedModuleExports |> Map.tryFind moduleName |> Option.defaultValue Map.empty
                                    let implDecls = availableDataTypeImplDecls |> Map.tryFind fullTypePath |> Option.defaultValue []
                                    implDecls
                                    |> List.collect (fun implDecl -> implDecl.methods)
                                    |> List.fold
                                        (fun (methodMap, diagAcc) methodDecl ->
                                            let exportName = $"{typeName}.{methodDecl.name}"
                                            match moduleExports |> Map.tryFind exportName with
                                            | Some exportInfo ->
                                                let isStatic =
                                                    match methodDecl.args with
                                                    | (:? Ast.FnArg.Named as thisArg) :: _ when thisArg.name = "this" -> false
                                                    | _ -> true
                                                Map.add methodDecl.name (exportInfo.symbolId, exportInfo.typ, isStatic) methodMap, diagAcc
                                            | None ->
                                                methodMap, diagAcc @ [ Diagnostic.Error(sprintf "Imported method '%s' for type '%s' was not found in module exports" methodDecl.name fullTypePath, methodDecl.span) ])
                                        (Map.empty, [])

                            let importedDefWithMethods =
                                { importedDef with
                                    methods = importedImplMethodMap }
                            Map.add aliasName importedDefWithMethods defs, diagnostics @ importedImplDiagnostics)
                    (Map.empty, [])

            let dataTypeDefs =
                importedDataTypeDefs
                |> Map.fold (fun defs aliasName importedDef -> Map.add aliasName importedDef defs) dataTypeDefs

            // impl 宣言のシグネチャを先に登録し、メソッド解決を安定化する。
            let dataTypeDefsWithMethods, implMethodDecls, implDiagnostics =
                resolvedModule.implDecls
                |> List.fold
                    (fun (defs, methodDecls, diagnostics) (typeSid, baseTypeSidOpt, byFieldNameOpt, implDecl) ->
                        match defs |> Map.tryFind implDecl.typeName with
                        | None ->
                            defs, methodDecls, diagnostics @ [ Diagnostic.Error(sprintf "Undefined impl target type '%s'" implDecl.typeName, implDecl.span) ]
                        | Some dataTypeDef ->
                            let foldResult =
                                implDecl.methods
                                |> List.fold
                                    (fun (methodMap, declAcc, diagAcc) methodDecl ->
                                        // impl メソッドをシンボル表へ登録し、instance/static 種別をメタデータに保持する。
                                        let registerImplMethod (isStatic: bool) =
                                            if Map.containsKey methodDecl.name methodMap then
                                                methodMap, declAcc, diagAcc @ [ Diagnostic.Error(sprintf "Duplicate method '%s' in impl '%s'" methodDecl.name implDecl.typeName, methodDecl.span) ]
                                            else
                                                let argTypes = methodDecl.args |> List.map bootstrapNameEnv.resolveArgType |> List.filter (fun t -> t <> TypeId.Unit)
                                                let retType = bootstrapNameEnv.resolveTypeExpr methodDecl.ret
                                                let methodType = TypeId.Fn(argTypes, retType)
                                                let methodSid = symbolTable.NextId()
                                                symbolTable.Add(methodSid, { name = $"{implDecl.typeName}.{methodDecl.name}"; typ = methodType; kind = SymbolKind.Local() })
                                                Map.add methodDecl.name (methodSid, methodType, isStatic) methodMap, (methodSid, methodDecl) :: declAcc, diagAcc

                                        match methodDecl.args with
                                        | (:? Ast.FnArg.Named as thisArg) :: _ ->
                                            let thisType = bootstrapNameEnv.resolveTypeExpr thisArg.typeExpr
                                            match thisArg.name, thisType with
                                            | "this", TypeId.Name thisTypeSid when thisTypeSid.id = typeSid.id -> registerImplMethod false
                                            | "this", _ ->
                                                methodMap, declAcc, diagAcc @ [ Diagnostic.Error("impl instance method '(this: ...)' must target the impl type", methodDecl.span) ]
                                            | _ ->
                                                registerImplMethod true
                                        | _ ->
                                            registerImplMethod true)
                                    (dataTypeDef.methods, [], [])

                            let methodMap, declAcc, diagAcc = foldResult
                            let updatedDefs =
                                defs
                                |> Map.add
                                    implDecl.typeName
                                    { dataTypeDef with
                                        methods = methodMap
                                        baseTypeSid = baseTypeSidOpt
                                        delegatedByFieldName = byFieldNameOpt }
                            updatedDefs, (List.rev declAcc) @ methodDecls, diagnostics @ diagAcc)
                    (dataTypeDefs, [], [])

            let nameEnv = NameEnv(symbolTable, resolvedModule.moduleScope, dataTypeDefsWithMethods, moduleExportView)

            resolvedModule.fnDecls
            |> List.iter (fun fnDecl -> methods.Add(ExprAnalyze.analyzeMethod nameEnv typeEnv fnDecl))

            implMethodDecls
            |> List.iter (fun (methodSid, methodDecl) ->
                methods.Add(ExprAnalyze.analyzeMethodCore nameEnv typeEnv methodSid methodDecl))

            // impl で確定した基底型情報を HIR.Type へ反映する。
            let baseTypeBySid =
                dataTypeDefsWithMethods
                |> Map.toSeq
                |> Seq.map (fun (_, def) -> def.typeSid.id, (def.baseTypeSid |> Option.map TypeId.Name))
                |> Map.ofSeq

            let typedTypes =
                types
                |> Seq.toList
                |> List.map (fun typ ->
                    let baseTypeOpt = baseTypeBySid |> Map.tryFind typ.sym.id |> Option.flatten
                    Hir.Type(typ.sym, baseTypeOpt, typ.fields, typ.methods))

            let untypedHirModule =
                Hir.Module(
                    resolvedModule.moduleName,
                    typedTypes,
                    fields |> Seq.toList,
                    methods |> Seq.toList,
                    resolvedModule.moduleScope
                )

            // IntelliSense 用に、エラーを含む場合でも可能な限り HIR を返す。
            let resolveAndImplDiagnostics = importedTypeDiagnostics @ implDiagnostics
            if resolveAndImplDiagnostics |> List.exists (fun d -> d.isError) then
                PhaseResult.failedWithValue untypedHirModule resolveAndImplDiagnostics
            else
                match Infer.inferModule (typeSubst, untypedHirModule) with
                | Result.Ok hir -> PhaseResult.succeeded hir resolveAndImplDiagnostics
                // 型推論に失敗した場合も、補完で使える部分 HIR を保持する。
                | Result.Error diagnostics ->
                    let allDiagnostics = resolveAndImplDiagnostics @ diagnostics
                    PhaseResult.failedWithValue untypedHirModule allDiagnostics
        | _ -> PhaseResult.failed [ Diagnostic.Error("Unknown analyze module failure", Atla.Core.Data.Span.Empty) ]

    /// 既存呼び出し向け互換 API。Atla モジュール import は外部から供給しない。
    let analyzeModule (symbolTable: SymbolTable, typeSubst: TypeSubst, moduleName: string, moduleAst: Ast.Module) : PhaseResult<Hir.Module> =
        analyzeModuleWithImports (symbolTable, typeSubst, moduleName, moduleAst, Set.empty, Set.empty, Map.empty, Map.empty, Map.empty)
