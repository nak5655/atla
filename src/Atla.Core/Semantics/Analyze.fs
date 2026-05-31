namespace Atla.Core.Semantics

open System.Collections.Generic
open System.Reflection
open Atla.Core.Syntax.Data
open Atla.Core.Semantics.Data
open Atla.Core.Semantics.Data.AnalyzeEnv

module Analyze =

    /// enum ルート型に埋め込む隠し tag フィールド名を決定する。
    let private enumTagFieldName (typeName: string) = $"{typeName}.__enum_tag"

    /// enum case ごとの隠し payload スロット名を決定する。
    let private enumPayloadFieldName (typeName: string) (caseName: string) = $"{typeName}.__enum_payload_{caseName}"

    /// enum case payload 用の隠し型名を決定する。
    let private enumPayloadTypeName (typeName: string) (caseName: string) = $"{typeName}.__enum_payload_{caseName}_type"

    let analyzeModuleWithImports
        (
            symbolTable: SymbolTable,
            typeSubst: TypeSubst,
            typeMetaFactory: TypeMetaFactory,
            moduleName: string,
            moduleAst: Ast.Module,
            sourceModuleNames: Set<string>,
            sourceTypeFullNames: Set<string>,
            dependencyModuleNames: Set<string>,
            dependencyTypeFullNames: Set<string>,
            availableTypeDecls: Map<string, Ast.Decl>,
            availableDataTypeImplDecls: Map<string, Ast.Decl.Impl list>,
            importedModuleExports: Map<string, Map<string, ModuleExport>>,
            importedDependencyTypeDefs: Map<string, DataTypeDef>
        )
        : PhaseResult<Hir.Module> =
        match
            Resolve.resolveModuleWithImports (
                symbolTable,
                moduleName,
                moduleAst,
                sourceModuleNames,
                sourceTypeFullNames,
                dependencyModuleNames,
                dependencyTypeFullNames
            )
        with
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

            // Std.lib は標準ライブラリのエントリモジュール識別名。Compile.fs の同名定数と一致させること。
            let stdPreludeModuleName = "Std.lib"
            let bootstrapNameEnv = NameEnv(symbolTable, resolvedModule.moduleScope, Map.empty, moduleExportView)
            // typeMetaFactory は複数モジュールをまたいで共有されるため、
            // 各モジュールで新しいファクトリを生成すると TypeSubst 上のメタ ID が衝突する。
            let typeEnv = TypeEnv(typeSubst, typeMetaFactory)

            // ─── Prelude 型の早期スコープ注入 ──────────────────────────────────────────────
            // data/enum 宣言のフィールド型解析（下記 dataTypeDefs）が Prelude 型（例: Opt<T>）を
            // 正しく解決できるよう、dataTypeDefs 構築前に Prelude 型をスコープへ宣言する。
            // Scope.DeclareType は冪等であるため、後続の importedTypeDefs 処理で重複登録しても安全。
            // ──────────────────────────────────────────────────────────────────────────────
            if moduleName <> stdPreludeModuleName then
                match importedModuleExports |> Map.tryFind stdPreludeModuleName with
                | None -> ()
                | Some preludeExports ->
                    preludeExports
                    |> Map.iter (fun key exportInfo ->
                        if key.StartsWith("type:") then
                            let typeName = key.Substring(5)
                            // 内部ペイロード型（ドットを含む名前）と明示的インポート済み型は除外する。
                            if not (typeName.Contains('.')) && not (resolvedModule.importedTypeAliases.ContainsKey(typeName)) then
                                resolvedModule.moduleScope.DeclareType(typeName, TypeId.Name exportInfo.symbolId))

            let fields = List<Hir.Field>()
            let methods = List<Hir.Method>()
            let types = List<Hir.Type>()

            /// 引数が `self` レシーバー（`fn foo self ...` で導入される推論引数）かを判定する。
            let isSelfReceiverArg (arg: Ast.FnArg) =
                match arg with
                | :? Ast.FnArg.Inferred as inferredArg -> inferredArg.name = "self"
                | _ -> false

            /// 先頭引数が `self` レシーバーである場合にインスタンスメソッドとして扱う。
            let isInstanceMethod (args: Ast.FnArg list) =
                match args with
                | firstArg :: _ when isSelfReceiverArg firstArg -> true
                | _ -> false

            /// 関数引数型を解決する。`self` 推論引数は呼び出し側コンテキストで確定した receiver 型を使い、
            /// それ以外の引数は通常の型注釈解決（bootstrapNameEnv.resolveArgType）に委譲する。
            let resolveArgTypeWithSelf (selfType: TypeId) (arg: Ast.FnArg) =
                match arg with
                | :? Ast.FnArg.Inferred as inferredArg when inferredArg.name = "self" -> selfType
                | _ -> bootstrapNameEnv.resolveArgType arg

            // data 宣言を型定義へ正規化し、後続の式解析で参照するメタデータを構築する。
            let dataTypeDefs =
                resolvedModule.dataDecls
                |> List.fold
                    (fun defs resolvedDataDecl ->
                        // 型パラメータをサブスコープに登録してフィールド型を解決する。
                        // TypeId.TypeVar はジェネリック型パラメータとして Gen.fs まで伝播し、
                        // GenericTypeParameterBuilder へ解決される。
                        let fieldNameEnv =
                            if resolvedDataDecl.typeParams.IsEmpty then
                                bootstrapNameEnv
                            else
                                let sub = bootstrapNameEnv.sub()
                                for paramName in resolvedDataDecl.typeParams do
                                    sub.scope.DeclareType(paramName, TypeId.TypeVar paramName)
                                sub
                        let resolvedFields =
                            resolvedDataDecl.decl.items
                            |> List.choose (fun item ->
                                match item with
                                | :? Ast.DataItem.Field as fieldItem ->
                                    let fieldType = fieldNameEnv.resolveTypeExpr fieldItem.typeExpr
                                    let fieldSid = symbolTable.NextId()
                                    symbolTable.Add(fieldSid, { name = $"{resolvedDataDecl.decl.name}.{fieldItem.name}"; typ = fieldType; kind = SymbolKind.Local() })
                                    Some { name = fieldItem.name; sid = fieldSid; typ = fieldType; isMutable = fieldItem.isMutable; span = fieldItem.span }
                                | _ -> None)

                        let hirFields =
                            resolvedFields
                            |> List.map (fun fieldDef ->
                                Hir.Field(fieldDef.sid, fieldDef.typ, Hir.Expr.Unit(fieldDef.span), fieldDef.span))
                        types.Add(Hir.Type(resolvedDataDecl.typeSid, false, None, resolvedDataDecl.typeParams, hirFields, []))
                        Map.add
                            resolvedDataDecl.decl.name
                            { typeSid = resolvedDataDecl.typeSid
                              baseType = None
                              delegatedByFieldName = None
                              typeParams = resolvedDataDecl.typeParams
                              fields = resolvedFields
                              hiddenFields = []
                              enumInfo = None
                              unionInfo = None
                              methods = Map.empty }
                            defs)
                    Map.empty

            // enum 宣言を root 型 + 隠し payload 型群へ正規化し、後続の式解析で参照するメタデータを構築する。
            let dataTypeDefs =
                resolvedModule.enumDecls
                |> List.fold
                    (fun defs resolvedEnumDecl ->
                        let typeName = resolvedEnumDecl.decl.name
                        // 型パラメータをサブスコープに登録してケースのフィールド型を解決する。
                        let caseFieldNameEnv =
                            if resolvedEnumDecl.typeParams.IsEmpty then
                                bootstrapNameEnv
                            else
                                let sub = bootstrapNameEnv.sub()
                                for paramName in resolvedEnumDecl.typeParams do
                                    sub.scope.DeclareType(paramName, TypeId.TypeVar paramName)
                                sub
                        let tagFieldSid = symbolTable.NextId()
                        let tagFieldDef =
                            let fieldName = enumTagFieldName typeName
                            symbolTable.Add(tagFieldSid, { name = fieldName; typ = TypeId.Int; kind = SymbolKind.Local() })
                            { name = fieldName
                              sid = tagFieldSid
                              typ = TypeId.Int
                              isMutable = false
                              span = resolvedEnumDecl.decl.span }

                        let enumCases, hiddenRootFields, payloadTypes =
                            resolvedEnumDecl.decl.cases
                            |> List.mapi (fun tag caseDecl ->
                                match caseDecl with
                                | :? Ast.EnumCase.Case as enumCase ->
                                    let payloadTypeName = enumPayloadTypeName typeName enumCase.name
                                    let payloadFieldName = enumPayloadFieldName typeName enumCase.name
                                    let payloadFieldDefs =
                                        enumCase.fields
                                        |> List.map (fun caseField ->
                                            let fieldSid = symbolTable.NextId()
                                            let fieldType = caseFieldNameEnv.resolveTypeExpr caseField.typeExpr
                                            let symbolName = $"{payloadTypeName}.{caseField.name}"
                                            symbolTable.Add(fieldSid, { name = symbolName; typ = fieldType; kind = SymbolKind.Local() })
                                            { name = caseField.name
                                              sid = fieldSid
                                              typ = fieldType
                                              isMutable = false
                                              span = caseField.span })

                                    match payloadFieldDefs with
                                    | [] ->
                                        { name = enumCase.name
                                          tag = tag
                                          payloadTypeSid = None
                                          payloadFieldSid = None
                                          fields = []
                                          span = enumCase.span },
                                        None,
                                        None
                                    | _ ->
                                        let payloadTypeSid = symbolTable.NextId()
                                        symbolTable.Add(payloadTypeSid, { name = payloadTypeName; typ = TypeId.Name payloadTypeSid; kind = SymbolKind.Local() })
                                        resolvedModule.moduleScope.DeclareType(payloadTypeName, TypeId.Name payloadTypeSid)

                                        let payloadRootFieldSid = symbolTable.NextId()
                                        symbolTable.Add(payloadRootFieldSid, { name = payloadFieldName; typ = TypeId.Name payloadTypeSid; kind = SymbolKind.Local() })

                                        let payloadRootFieldDef =
                                            { name = payloadFieldName
                                              sid = payloadRootFieldSid
                                              typ = TypeId.Name payloadTypeSid
                                              isMutable = false
                                              span = enumCase.span }

                                        let payloadType =
                                            let payloadFields =
                                                payloadFieldDefs
                                                |> List.map (fun fieldDef ->
                                                    Hir.Field(fieldDef.sid, fieldDef.typ, Hir.Expr.Unit(fieldDef.span), fieldDef.span))
                                            // payload 型にも型パラメータを伝播する（GenType の型引数解決に必要）。
                                            Hir.Type(payloadTypeSid, false, None, resolvedEnumDecl.typeParams, payloadFields, [])

                                        { name = enumCase.name
                                          tag = tag
                                          payloadTypeSid = Some payloadTypeSid
                                          payloadFieldSid = Some payloadRootFieldSid
                                          fields = payloadFieldDefs
                                          span = enumCase.span },
                                        Some payloadRootFieldDef,
                                        Some payloadType
                                | _ ->
                                    { name = $"error_case_{tag}"
                                      tag = tag
                                      payloadTypeSid = None
                                      payloadFieldSid = None
                                      fields = []
                                      span = caseDecl.span },
                                    None,
                                    None)
                            |> List.fold
                                (fun (casesAcc, hiddenFieldsAcc, payloadTypesAcc) (caseDef, hiddenFieldOpt, payloadTypeOpt) ->
                                    let hiddenFieldsAcc' = hiddenFieldOpt |> Option.map (fun fieldDef -> hiddenFieldsAcc @ [ fieldDef ]) |> Option.defaultValue hiddenFieldsAcc
                                    let payloadTypesAcc' = payloadTypeOpt |> Option.map (fun payloadType -> payloadTypesAcc @ [ payloadType ]) |> Option.defaultValue payloadTypesAcc
                                    casesAcc @ [ caseDef ], hiddenFieldsAcc', payloadTypesAcc')
                                ([], [ tagFieldDef ], [])

                        payloadTypes |> List.iter types.Add

                        let rootFields =
                            hiddenRootFields
                            |> List.map (fun fieldDef ->
                                Hir.Field(fieldDef.sid, fieldDef.typ, Hir.Expr.Unit(fieldDef.span), fieldDef.span))

                        types.Add(Hir.Type(resolvedEnumDecl.typeSid, false, None, resolvedEnumDecl.typeParams, rootFields, []))

                        Map.add
                            typeName
                            { typeSid = resolvedEnumDecl.typeSid
                              baseType = None
                              delegatedByFieldName = None
                              typeParams = resolvedEnumDecl.typeParams
                              fields = []
                              hiddenFields = hiddenRootFields
                              enumInfo =
                                  Some
                                      { hiddenTagField = tagFieldDef
                                        cases = enumCases }
                              unionInfo = None
                              methods = Map.empty }
                            defs)
                    dataTypeDefs

            // union 宣言を abstract class（ルート型）+ 派生クラス（バリアント）群へ正規化する。
            // - ルート型: union 本体のフィールドを持つ abstract class（isAbstract=true）。
            // - 各バリアント: baseType=ルート型 の具象クラス。自身のフィールドのみを保持し、
            //   union フィールドは継承で共有する（メンバー解決が baseType 連鎖を辿る）。
            let dataTypeDefs =
                resolvedModule.unionDecls
                |> List.fold
                    (fun defs resolvedUnionDecl ->
                        let unionName = resolvedUnionDecl.decl.name
                        let unionSid = resolvedUnionDecl.typeSid

                        // 型パラメータをサブスコープへ登録してフィールド型を解決する（Phase 1 では非ジェネリック中心）。
                        let fieldNameEnv =
                            if resolvedUnionDecl.typeParams.IsEmpty then
                                bootstrapNameEnv
                            else
                                let sub = bootstrapNameEnv.sub()
                                for paramName in resolvedUnionDecl.typeParams do
                                    sub.scope.DeclareType(paramName, TypeId.TypeVar paramName)
                                sub

                        // union 本体直下のフィールド（全バリアントが継承する共有フィールド）。
                        let unionFieldDefs =
                            resolvedUnionDecl.decl.fields
                            |> List.choose (fun item ->
                                match item with
                                | :? Ast.DataItem.Field as fieldItem ->
                                    let fieldType = fieldNameEnv.resolveTypeExpr fieldItem.typeExpr
                                    let fieldSid = symbolTable.NextId()
                                    symbolTable.Add(fieldSid, { name = $"{unionName}.{fieldItem.name}"; typ = fieldType; kind = SymbolKind.Local() })
                                    Some { name = fieldItem.name; sid = fieldSid; typ = fieldType; isMutable = fieldItem.isMutable; span = fieldItem.span }
                                | _ -> None)

                        // ルート型（abstract class）を生成する。
                        let unionRootHirFields =
                            unionFieldDefs
                            |> List.map (fun fieldDef -> Hir.Field(fieldDef.sid, fieldDef.typ, Hir.Expr.Unit(fieldDef.span), fieldDef.span))
                        types.Add(Hir.Type(unionSid, false, true, None, resolvedUnionDecl.typeParams, unionRootHirFields, []))

                        // 各バリアントの DataTypeDef を構築し、defs へ追加する。
                        let variantInfos, defsWithVariants =
                            resolvedUnionDecl.decl.variants
                            |> List.fold
                                (fun (variantAcc, defsAcc) variant ->
                                    let variantNameOpt, ownFieldItems, objectFieldInits =
                                        match variant with
                                        | :? Ast.Decl.Data as structVariant -> Some structVariant.name, structVariant.items, None
                                        | :? Ast.Decl.Object as objVariant ->
                                            // object バリアントは自身のフィールドを持たず、継承フィールドの初期値のみを供給する。
                                            let inits =
                                                objVariant.fieldInits
                                                |> List.choose (fun init ->
                                                    match init with
                                                    | :? Ast.DataInitField.Field as f -> Some(f.name, f.value)
                                                    | _ -> None)
                                            Some objVariant.name, [], Some inits
                                        | _ -> None, [], None
                                    match variantNameOpt |> Option.bind (fun n -> resolvedUnionDecl.variantSids |> Map.tryFind n |> Option.map (fun sid -> n, sid)) with
                                    | None -> variantAcc, defsAcc
                                    | Some (variantName, variantSid) ->
                                        let qualifiedName = sprintf "%s'%s" unionName variantName
                                        // バリアント自身のフィールド（struct バリアントの items）を解決する。
                                        let ownFieldDefs =
                                            ownFieldItems
                                            |> List.choose (fun item ->
                                                match item with
                                                | :? Ast.DataItem.Field as fieldItem ->
                                                    let fieldType = fieldNameEnv.resolveTypeExpr fieldItem.typeExpr
                                                    let fieldSid = symbolTable.NextId()
                                                    symbolTable.Add(fieldSid, { name = $"{qualifiedName}.{fieldItem.name}"; typ = fieldType; kind = SymbolKind.Local() })
                                                    Some { name = fieldItem.name; sid = fieldSid; typ = fieldType; isMutable = fieldItem.isMutable; span = fieldItem.span }
                                                | _ -> None)
                                        let variantHirFields =
                                            ownFieldDefs
                                            |> List.map (fun fieldDef -> Hir.Field(fieldDef.sid, fieldDef.typ, Hir.Expr.Unit(fieldDef.span), fieldDef.span))
                                        types.Add(Hir.Type(variantSid, false, false, Some(TypeId.Name unionSid), resolvedUnionDecl.typeParams, variantHirFields, []))
                                        let variantDef =
                                            { typeSid = variantSid
                                              baseType = Some(TypeId.Name unionSid)
                                              delegatedByFieldName = None
                                              typeParams = resolvedUnionDecl.typeParams
                                              fields = ownFieldDefs
                                              hiddenFields = []
                                              enumInfo = None
                                              unionInfo = None
                                              methods = Map.empty }
                                        let variantInfo = { name = variantName; typeSid = variantSid; objectFieldInits = objectFieldInits; span = variant.span }
                                        variantAcc @ [ variantInfo ], Map.add qualifiedName variantDef defsAcc)
                                ([], defs)

                        // ルート型の DataTypeDef を登録する。
                        Map.add
                            unionName
                            { typeSid = unionSid
                              baseType = None
                              delegatedByFieldName = None
                              typeParams = resolvedUnionDecl.typeParams
                              fields = unionFieldDefs
                              hiddenFields = []
                              enumInfo = None
                              unionInfo = Some { isExtendable = resolvedUnionDecl.decl.isExtendable; variants = variantInfos }
                              methods = Map.empty }
                            defsWithVariants)
                    dataTypeDefs

            // role 宣言を HIR インターフェイス型として処理する。
            // role の各メソッドシグネチャをシンボルテーブルへ登録し、
            // isInterface=true の Hir.Type を生成する。
            // method の body はプレースホルダー（Hir.Expr.Unit）とし、Gen 側で本体なし abstract として処理する。
            for resolvedRoleDecl in resolvedModule.roleDecls do
                let roleName = resolvedRoleDecl.decl.name
                let roleSpan = resolvedRoleDecl.decl.span
                let hirMethods =
                    resolvedRoleDecl.decl.methods
                    |> List.map (fun roleFn ->
                        let argTypes =
                            roleFn.args
                            |> List.map (resolveArgTypeWithSelf (TypeId.Name resolvedRoleDecl.typeSid))
                            |> (fun raw ->
                                match roleFn.args, raw with
                                | [ (:? Ast.FnArg.Unit) ], [ TypeId.Unit ] -> []
                                | _ -> raw)
                        let retType = bootstrapNameEnv.resolveTypeExpr roleFn.ret
                        let methodType = TypeId.Fn(argTypes, retType)
                        let methodSid = symbolTable.NextId()
                        symbolTable.Add(methodSid, { name = $"{roleName}.{roleFn.name}"; typ = methodType; kind = SymbolKind.Local() })
                        // abstract メソッドを引数 SID と共に構築する。ボディはプレースホルダー。
                        let argSids =
                            roleFn.args
                            |> List.mapi (fun i arg ->
                                match arg with
                                | :? Ast.FnArg.Named as namedArg ->
                                    let argType = argTypes.[i]
                                    let argSid = symbolTable.NextId()
                                    symbolTable.Add(argSid, { name = namedArg.name; typ = argType; kind = SymbolKind.Arg() })
                                    Some (argSid, argType)
                                | :? Ast.FnArg.Inferred as inferredArg ->
                                    let argType = argTypes.[i]
                                    let argSid = symbolTable.NextId()
                                    symbolTable.Add(argSid, { name = inferredArg.name; typ = argType; kind = SymbolKind.Arg() })
                                    Some (argSid, argType)
                                | :? Ast.FnArg.Unit -> None
                                | _ -> None)
                            |> List.choose id
                        Hir.Method(methodSid, argSids, Hir.Expr.Unit(roleSpan), methodType, None, false, roleSpan))
                types.Add(Hir.Type(resolvedRoleDecl.typeSid, true, None, [], [], hirMethods))

            // Std.Prelude に含まれるジェネリック enum 等を暗黙インポートとして解決する。
            // Std.Prelude 自体のコンパイル中と、既に明示的に同名型をインポートしているモジュールは除外する。
            let allImportedTypeAliases =
                if moduleName = stdPreludeModuleName then
                    resolvedModule.importedTypeAliases
                else
                    match importedModuleExports |> Map.tryFind stdPreludeModuleName with
                    | None -> resolvedModule.importedTypeAliases
                    | Some preludeExports ->
                        // Prelude がエクスポートする型（"type:{Name}"）のうち、
                        // 内部ペイロード型（ドットを含む名前）を除いた公開型名を収集する。
                        preludeExports
                        |> Map.toList
                        |> List.choose (fun (key, _) ->
                            if key.StartsWith("type:") then
                                let typeName = key.Substring(5)
                                if not (typeName.Contains('.')) then
                                    // 明示的インポートがない Prelude 型はすべて自動注入する。
                                    // スコープへの早期注入済みでも importedTypeDefs のメタデータ構築は必要。
                                    let notExplicit = not (resolvedModule.importedTypeAliases.ContainsKey(typeName))
                                    if notExplicit then
                                        Some (typeName, $"{stdPreludeModuleName}.{typeName}")
                                    else None
                                else None
                            else None)
                        |> List.fold (fun aliases (name, fullPath) -> Map.add name fullPath aliases) resolvedModule.importedTypeAliases

            let importedTypeDefs, importedTypeDiagnostics =
                allImportedTypeAliases
                |> Map.toList
                |> List.fold
                    (fun (defs, diagnostics) (aliasName, fullTypePath) ->
                        match availableTypeDecls.TryFind(fullTypePath) with
                        | None when importedDependencyTypeDefs.ContainsKey(fullTypePath) ->
                            let importedDef = importedDependencyTypeDefs[fullTypePath]
                            resolvedModule.moduleScope.DeclareType(aliasName, TypeId.Name importedDef.typeSid)
                            Map.add aliasName importedDef defs, diagnostics
                        | None ->
                            defs, diagnostics @ [ Diagnostic.Error(sprintf "Imported type '%s' was not found" fullTypePath, Atla.Core.Data.Span.Empty) ]
                        | Some typeDecl ->
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

                            let importedDef =
                                match typeDecl with
                                | :? Ast.Decl.Data as dataDecl ->
                                    let resolvedFields =
                                        dataDecl.items
                                        |> List.choose (fun item ->
                                            match item with
                                            | :? Ast.DataItem.Field as fieldItem ->
                                                let exportedFieldInfoOpt =
                                                    sourceModuleExports |> Map.tryFind $"field:{typeNameForLookup}.{fieldItem.name}"
                                                let fieldType =
                                                    match exportedFieldInfoOpt with
                                                    | Some exportInfo -> exportInfo.typ
                                                    | None -> bootstrapNameEnv.resolveTypeExpr fieldItem.typeExpr
                                                let fieldSid =
                                                    match exportedFieldInfoOpt with
                                                    | Some exportInfo -> exportInfo.symbolId
                                                    | None ->
                                                        let newSid = symbolTable.NextId()
                                                        symbolTable.Add(newSid, { name = $"{aliasName}.{fieldItem.name}"; typ = fieldType; kind = SymbolKind.Local() })
                                                        newSid
                                                Some { name = fieldItem.name; sid = fieldSid; typ = fieldType; isMutable = fieldItem.isMutable; span = fieldItem.span }
                                            | _ -> None)

                                    if not isReusingExistingType then
                                        let hirFields =
                                            resolvedFields
                                            |> List.map (fun fieldDef ->
                                                Hir.Field(fieldDef.sid, fieldDef.typ, Hir.Expr.Unit(fieldDef.span), fieldDef.span))
                                        types.Add(Hir.Type(typeSid, false, None, [], hirFields, []))

                                    { typeSid = typeSid
                                      baseType = None
                                      delegatedByFieldName = None
                                      typeParams = []
                                      fields = resolvedFields
                                      hiddenFields = []
                                      enumInfo = None
                                      unionInfo = None
                                      methods = Map.empty }
                                | :? Ast.Decl.Enum as enumDecl ->
                                    let tagFieldName = enumTagFieldName typeNameForLookup
                                    let tagFieldExport = sourceModuleExports |> Map.tryFind $"field:{tagFieldName}"
                                    let hiddenTagField =
                                        match tagFieldExport with
                                        | Some exportInfo ->
                                            { name = tagFieldName
                                              sid = exportInfo.symbolId
                                              typ = exportInfo.typ
                                              isMutable = false
                                              span = enumDecl.span }
                                        | None ->
                                            let sid = symbolTable.NextId()
                                            symbolTable.Add(sid, { name = tagFieldName; typ = TypeId.Int; kind = SymbolKind.Local() })
                                            { name = tagFieldName
                                              sid = sid
                                              typ = TypeId.Int
                                              isMutable = false
                                              span = enumDecl.span }

                                    let enumCases, hiddenFields, payloadTypes =
                                        enumDecl.cases
                                        |> List.mapi (fun tag caseDecl ->
                                            match caseDecl with
                                            | :? Ast.EnumCase.Case as enumCase ->
                                                let payloadTypeName = enumPayloadTypeName typeNameForLookup enumCase.name
                                                let payloadSlotName = enumPayloadFieldName typeNameForLookup enumCase.name
                                                let payloadFieldDefs =
                                                    enumCase.fields
                                                    |> List.map (fun caseField ->
                                                        let exportInfoOpt =
                                                            sourceModuleExports |> Map.tryFind $"field:{payloadTypeName}.{caseField.name}"
                                                        let fieldType =
                                                            match exportInfoOpt with
                                                            | Some exportInfo -> exportInfo.typ
                                                            | None -> bootstrapNameEnv.resolveTypeExpr caseField.typeExpr
                                                        let fieldSid =
                                                            match exportInfoOpt with
                                                            | Some exportInfo -> exportInfo.symbolId
                                                            | None ->
                                                                let sid = symbolTable.NextId()
                                                                symbolTable.Add(sid, { name = $"{payloadTypeName}.{caseField.name}"; typ = fieldType; kind = SymbolKind.Local() })
                                                                sid
                                                        { name = caseField.name
                                                          sid = fieldSid
                                                          typ = fieldType
                                                          isMutable = false
                                                          span = caseField.span })

                                                match payloadFieldDefs with
                                                | [] ->
                                                    { name = enumCase.name
                                                      tag = tag
                                                      payloadTypeSid = None
                                                      payloadFieldSid = None
                                                      fields = []
                                                      span = enumCase.span },
                                                    None,
                                                    None
                                                | _ ->
                                                    let payloadTypeSid =
                                                        match sourceModuleExports |> Map.tryFind $"type:{payloadTypeName}" with
                                                        | Some exportInfo -> exportInfo.symbolId
                                                        | None ->
                                                            let sid = symbolTable.NextId()
                                                            symbolTable.Add(sid, { name = payloadTypeName; typ = TypeId.Name sid; kind = SymbolKind.Local() })
                                                            sid
                                                    let payloadSlotDef =
                                                        match sourceModuleExports |> Map.tryFind $"field:{payloadSlotName}" with
                                                        | Some exportInfo ->
                                                            { name = payloadSlotName
                                                              sid = exportInfo.symbolId
                                                              typ = exportInfo.typ
                                                              isMutable = false
                                                              span = enumCase.span }
                                                        | None ->
                                                            let sid = symbolTable.NextId()
                                                            symbolTable.Add(sid, { name = payloadSlotName; typ = TypeId.Name payloadTypeSid; kind = SymbolKind.Local() })
                                                            { name = payloadSlotName
                                                              sid = sid
                                                              typ = TypeId.Name payloadTypeSid
                                                              isMutable = false
                                                              span = enumCase.span }
                                                    let payloadTypeOpt =
                                                        if isReusingExistingType then
                                                            None
                                                        else
                                                            let payloadFields =
                                                                payloadFieldDefs
                                                                |> List.map (fun fieldDef ->
                                                                    Hir.Field(fieldDef.sid, fieldDef.typ, Hir.Expr.Unit(fieldDef.span), fieldDef.span))
                                                            Some (Hir.Type(payloadTypeSid, false, None, [], payloadFields, []))
                                                    { name = enumCase.name
                                                      tag = tag
                                                      payloadTypeSid = Some payloadTypeSid
                                                      payloadFieldSid = Some payloadSlotDef.sid
                                                      fields = payloadFieldDefs
                                                      span = enumCase.span },
                                                    Some payloadSlotDef,
                                                    payloadTypeOpt
                                            | _ ->
                                                { name = $"error_case_{tag}"
                                                  tag = tag
                                                  payloadTypeSid = None
                                                  payloadFieldSid = None
                                                  fields = []
                                                  span = caseDecl.span },
                                                None,
                                                None)
                                        |> List.fold
                                            (fun (caseAcc, hiddenAcc, payloadTypeAcc) (caseDef, hiddenFieldOpt, payloadTypeOpt) ->
                                                let hiddenAcc' = hiddenFieldOpt |> Option.map (fun fieldDef -> hiddenAcc @ [ fieldDef ]) |> Option.defaultValue hiddenAcc
                                                let payloadTypeAcc' = payloadTypeOpt |> Option.map (fun payloadType -> payloadTypeAcc @ [ payloadType ]) |> Option.defaultValue payloadTypeAcc
                                                caseAcc @ [ caseDef ], hiddenAcc', payloadTypeAcc')
                                            ([], [ hiddenTagField ], [])

                                    if not isReusingExistingType then
                                        payloadTypes |> List.iter types.Add
                                        let rootFields =
                                            hiddenFields
                                            |> List.map (fun fieldDef ->
                                                Hir.Field(fieldDef.sid, fieldDef.typ, Hir.Expr.Unit(fieldDef.span), fieldDef.span))
                                        types.Add(Hir.Type(typeSid, false, None, [], rootFields, []))

                                    { typeSid = typeSid
                                      baseType = None
                                      delegatedByFieldName = None
                                      typeParams = []
                                      fields = []
                                      hiddenFields = hiddenFields
                                      enumInfo =
                                          Some
                                              { hiddenTagField = hiddenTagField
                                                cases = enumCases }
                                      unionInfo = None
                                      methods = Map.empty }
                                | _ ->
                                    { typeSid = typeSid
                                      baseType = None
                                      delegatedByFieldName = None
                                      typeParams = []
                                      fields = []
                                      hiddenFields = []
                                      enumInfo = None
                                      unionInfo = None
                                      methods = Map.empty }
                            let importedImplMethodMap, importedImplDiagnostics =
                                let lastDot = fullTypePath.LastIndexOf('.')
                                if lastDot <= 0 then
                                    Map.empty, []
                                else
                                    let moduleName = fullTypePath.Substring(0, lastDot)
                                    let typeName = fullTypePath.Substring(lastDot + 1)
                                    let moduleExports = importedModuleExports |> Map.tryFind moduleName |> Option.defaultValue Map.empty
                                    let allImportedExports = importedModuleExports |> Map.toList |> List.map snd
                                    let implDecls = availableDataTypeImplDecls |> Map.tryFind fullTypePath |> Option.defaultValue []
                                    implDecls
                                    |> List.collect (fun implDecl -> implDecl.methods)
                                    |> List.fold
                                        (fun (methodMap, diagAcc) methodDecl ->
                                            // imported impl method の export 名はフェーズ進化で揺れる可能性があるため、
                                            // 期待キー（Type.Method）を優先しつつ互換キーへ決定的順序でフォールバックする。
                                            let exportKeyCandidates =
                                                [ $"{typeName}.{methodDecl.name}"
                                                  $"{fullTypePath}.{methodDecl.name}"
                                                  methodDecl.name ]

                                            let exportInfoOpt =
                                                exportKeyCandidates
                                                |> List.tryPick (fun key ->
                                                    match moduleExports |> Map.tryFind key with
                                                    | Some exportInfo -> Some exportInfo
                                                    | None ->
                                                        allImportedExports
                                                        |> List.tryPick (fun exports -> exports |> Map.tryFind key))

                                            match exportInfoOpt with
                                            | Some exportInfo ->
                                                let isStatic = not (isInstanceMethod methodDecl.args)
                                                Map.add methodDecl.name (exportInfo.symbolId, exportInfo.typ, isStatic) methodMap, diagAcc
                                            | None ->
                                                methodMap, diagAcc @ [ Diagnostic.Error(sprintf "Imported method '%s' for type '%s' was not found in module exports" methodDecl.name fullTypePath, methodDecl.span) ])
                                        (Map.empty, [])

                            // imported impl 宣言から `for`（基底型）と `by`（委譲先フィールド）を抽出し、
                            // インスタンスメンバー解決が定義元モジュールと同じ情報で実行できるようにする。
                            let importedBaseTypeOpt, importedDelegatedByFieldNameOpt, importedDelegationDiagnostics =
                                let implDecls = availableDataTypeImplDecls |> Map.tryFind fullTypePath |> Option.defaultValue []

                                // `impl X as DotNetBase` パターンを先にチェックする。
                                // この場合、モジュールエクスポートの "implBase:{TypeName}" キーから .NET 基底型を復元する。
                                // asTypeName.IsSome の有無のみを確認し、宣言の内容は不要なため Some _ でマッチする。
                                let hasAsImpl = implDecls |> List.exists (fun implDecl -> implDecl.asTypeName.IsSome)
                                if hasAsImpl then
                                    let baseTypeOpt =
                                        sourceModuleExports
                                        |> Map.tryFind $"implBase:{typeNameForLookup}"
                                        |> Option.map (fun exportInfo -> exportInfo.typ)
                                    baseTypeOpt, None, []
                                else

                                let preferredDelegatedImplOpt =
                                    implDecls
                                    |> List.tryFind (fun implDecl -> implDecl.byFieldName.IsSome && implDecl.forTypeName.IsSome)

                                match preferredDelegatedImplOpt with
                                | None -> None, None, []
                                | Some delegatedImplDecl ->
                                    let resolvedBaseTypeOpt =
                                        match delegatedImplDecl.forTypeName with
                                        | None -> None
                                        | Some forTypeName ->
                                            match resolvedModule.moduleScope.ResolveType(forTypeName) with
                                            | Some(TypeId.Name baseTypeSid) -> Some (TypeId.Name baseTypeSid)
                                            | _ ->
                                                match sourceModuleExports |> Map.tryFind $"type:{forTypeName}" with
                                                | Some exportInfo -> Some (TypeId.Name exportInfo.symbolId)
                                                | None -> None

                                    let delegatedByFieldNameOpt, delegationDiagnostics =
                                        match delegatedImplDecl.byFieldName with
                                        | None -> None, []
                                        | Some byFieldName ->
                                            let hasField = importedDef.fields |> List.exists (fun fieldDef -> fieldDef.name = byFieldName)
                                            if hasField then
                                                Some byFieldName, []
                                            else
                                                None, [ Diagnostic.Error(sprintf "Delegate field '%s' is not defined in imported data '%s'" byFieldName aliasName, delegatedImplDecl.span) ]

                                    resolvedBaseTypeOpt, delegatedByFieldNameOpt, delegationDiagnostics

                            let importedDefWithMethods =
                                { importedDef with
                                    baseType = importedBaseTypeOpt
                                    delegatedByFieldName = importedDelegatedByFieldNameOpt
                                    methods = importedImplMethodMap }
                            Map.add aliasName importedDefWithMethods defs, diagnostics @ importedImplDiagnostics @ importedDelegationDiagnostics)
                    (Map.empty, [])

            let dataTypeDefs =
                importedTypeDefs
                |> Map.fold (fun defs aliasName importedDef -> Map.add aliasName importedDef defs) dataTypeDefs

            // impl 宣言のシグネチャを先に登録し、メソッド解決を安定化する。
            let dataTypeDefsWithMethods, implMethodDecls, implDiagnostics =
                resolvedModule.implDecls
                |> List.fold
                    (fun (defs, methodDecls, diagnostics) (typeSid, baseTypeOpt, byFieldNameOpt, implDecl) ->
                        // `impl X for Y` のとき Y がターゲットデータ型。`impl T` のときは T 自身。
                        let targetTypeName = implDecl.forTypeName |> Option.defaultValue implDecl.typeName
                        match defs |> Map.tryFind targetTypeName with
                        | None ->
                            defs, methodDecls, diagnostics @ [ Diagnostic.Error(sprintf "Undefined impl target type '%s'" targetTypeName, implDecl.span) ]
                        | Some dataTypeDef ->
                            // 型パラメータをサブスコープに登録し、メソッドの引数型・戻り型解決に使用する。
                            let implNameEnv =
                                if implDecl.typeParams.IsEmpty then
                                    bootstrapNameEnv
                                else
                                    let sub = bootstrapNameEnv.sub()
                                    for paramName in implDecl.typeParams do
                                        sub.scope.DeclareType(paramName, TypeId.TypeVar paramName)
                                    sub
                            let foldResult =
                                implDecl.methods
                                |> List.fold
                                    (fun (methodMap, declAcc, diagAcc) methodDecl ->
                                        // impl メソッドをシンボル表へ登録し、instance/static 種別をメタデータに保持する。
                                        let registerImplMethod (isStatic: bool) =
                                            if Map.containsKey methodDecl.name methodMap then
                                                methodMap, declAcc, diagAcc @ [ Diagnostic.Error(sprintf "Duplicate method '%s' in impl '%s'" methodDecl.name targetTypeName, methodDecl.span) ]
                                            else
                                                let argTypes =
                                                    methodDecl.args
                                                    |> List.map (resolveArgTypeWithSelf (TypeId.Name typeSid))
                                                    |> List.filter (fun t -> t <> TypeId.Unit)
                                                let retType =
                                                    let declared = implNameEnv.resolveTypeExpr methodDecl.ret
                                                    if methodDecl.isAsync then TypeId.wrapInTask declared else declared
                                                let methodType = TypeId.Fn(argTypes, retType)
                                                let methodSid = symbolTable.NextId()
                                                symbolTable.Add(methodSid, { name = $"{targetTypeName}.{methodDecl.name}"; typ = methodType; kind = SymbolKind.Local() })
                                                // 静的 impl メソッドをモジュールスコープへ登録する。
                                                // これにより、同モジュール内から addButton のようなベアネームで参照できる。
                                                if isStatic then
                                                    resolvedModule.moduleScope.DeclareVar(methodDecl.name, methodSid)

                                                // override 修飾子が付いている場合、親 .NET クラスの該当 virtual メソッドを解決する。
                                                // - 親クラスが取得できない（`impl A as B` 以外）のは Resolve でエラー済みなので None。
                                                // - static メソッドへの override はインスタンスメソッド限定なのでエラー。
                                                // - 名前 + arity（明示引数数）一致の候補を検索し、0/2 件以上ならエラー。
                                                let overrideTarget, overrideDiags =
                                                    if not methodDecl.isOverride then
                                                        None, []
                                                    elif isStatic then
                                                        None, [
                                                            Diagnostic.Error(
                                                                sprintf "'override' is only allowed on instance methods (with 'self' receiver); method '%s' in impl '%s'"
                                                                    methodDecl.name targetTypeName,
                                                                methodDecl.span) ]
                                                    else
                                                        match baseTypeOpt with
                                                        | Some (TypeId.Native sysType) when not (obj.ReferenceEquals(sysType, null)) ->
                                                            let methodName = methodDecl.name
                                                            // 明示引数数（self / Unit を除外）
                                                            let arity =
                                                                methodDecl.args
                                                                |> List.filter (fun arg ->
                                                                    match arg with
                                                                    | :? Ast.FnArg.Inferred -> false  // self
                                                                    | :? Ast.FnArg.Unit -> false       // ()
                                                                    | _ -> true)
                                                                |> List.length
                                                            let candidates =
                                                                NativeInterop.getOverridableInstanceMethods sysType
                                                                |> List.filter (fun mi ->
                                                                    mi.Name = methodName
                                                                    && mi.GetParameters().Length = arity)
                                                            match candidates with
                                                            | [] ->
                                                                None, [
                                                                    Diagnostic.Error(
                                                                        sprintf "No overridable method '%s' (arity %d) found in base class '%s' of impl '%s'"
                                                                            methodName arity sysType.FullName targetTypeName,
                                                                        methodDecl.span) ]
                                                            | [ mi ] -> Some mi, []
                                                            | _ ->
                                                                None, [
                                                                    Diagnostic.Error(
                                                                        sprintf "Ambiguous override target: multiple virtual methods named '%s' (arity %d) exist in '%s'"
                                                                            methodName arity sysType.FullName,
                                                                        methodDecl.span) ]
                                                        | _ ->
                                                            // Resolve で既にエラーが報告されているのでここでは追加しない。
                                                            None, []

                                                // declAcc には (メソッドSID, 宣言, ターゲット型SID, isStatic, overrideTarget) を積む。
                                                Map.add methodDecl.name (methodSid, methodType, isStatic) methodMap,
                                                (methodSid, methodDecl, dataTypeDef.typeSid, isStatic, overrideTarget) :: declAcc,
                                                diagAcc @ overrideDiags

                                        if isInstanceMethod methodDecl.args then
                                            registerImplMethod false
                                        else
                                            registerImplMethod true)
                                    (dataTypeDef.methods, [], [])

                            let methodMap, declAcc, diagAcc = foldResult
                            let updatedDefs =
                                defs
                                |> Map.add
                                    targetTypeName
                                    { dataTypeDef with
                                        methods = methodMap
                                        baseType = baseTypeOpt
                                        delegatedByFieldName = byFieldNameOpt }
                            updatedDefs, (List.rev declAcc) @ methodDecls, diagnostics @ diagAcc)
                    (dataTypeDefs, [], [])

            let nameEnv = NameEnv(symbolTable, resolvedModule.moduleScope, dataTypeDefsWithMethods, moduleExportView)

            resolvedModule.fnDecls
            |> List.iter (fun fnDecl -> methods.Add(ExprAnalyze.analyzeMethod nameEnv typeEnv fnDecl))

            // インスタンス impl メソッドはターゲット型の Hir.Type.methods へルーティングし、
            // CIL インスタンスメソッドとしてコンパイルする。
            // 静的 impl メソッドは従来どおりモジュールレベルメソッド（Globals 型の static）として追加する。
            let instanceImplMethodsByType = System.Collections.Generic.Dictionary<int, Hir.Method list>()

            implMethodDecls
            |> List.iter (fun (methodSid, methodDecl, targetTypeSid, isStatic, overrideTarget) ->
                let hirMethod = ExprAnalyze.analyzeMethodCoreWithOverride nameEnv typeEnv methodSid methodDecl overrideTarget
                if isStatic then
                    methods.Add(hirMethod)
                else
                    let existing =
                        match instanceImplMethodsByType.TryGetValue(targetTypeSid.id) with
                        | true, ms -> ms
                        | false, _ -> []
                    instanceImplMethodsByType.[targetTypeSid.id] <- existing @ [ hirMethod ])

            // impl で確定した基底型情報を HIR.Type へ反映する。
            let baseTypeById =
                dataTypeDefsWithMethods
                |> Map.toSeq
                |> Seq.map (fun (_, def) -> def.typeSid.id, def.baseType)
                |> Map.ofSeq

            let typedTypes =
                types
                |> Seq.toList
                |> List.map (fun typ ->
                    // interface 型（role）は baseType を変更しない。data 型のみ impl で確定した baseType を反映する。
                    let baseTypeOpt =
                        if typ.isInterface then typ.baseType
                        else baseTypeById |> Map.tryFind typ.sym.id |> Option.flatten
                    // インスタンス impl メソッドをターゲット型の methods に追加する。
                    // これにより CIL インスタンスメソッドとしてコンパイルされる。
                    let instanceImplMethods =
                        match instanceImplMethodsByType.TryGetValue(typ.sym.id) with
                        | true, ms -> ms
                        | false, _ -> []
                    Hir.Type(typ.sym, typ.isInterface, typ.isAbstract, baseTypeOpt, typ.typeParams, typ.fields, typ.methods @ instanceImplMethods))

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
        analyzeModuleWithImports (
            symbolTable,
            typeSubst,
            TypeMetaFactory(),
            moduleName,
            moduleAst,
            Set.empty,
            Set.empty,
            Set.empty,
            Set.empty,
            Map.empty,
            Map.empty,
            Map.empty,
            Map.empty
        )
