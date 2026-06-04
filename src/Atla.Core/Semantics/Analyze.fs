namespace Atla.Core.Semantics

open System.Collections.Generic
open System.Reflection
open Atla.Core.Syntax.Data
open Atla.Core.Semantics.Data
open Atla.Core.Semantics.Data.AnalyzeEnv

module Analyze =

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
            /// それ以外の引数は与えられた `env` で型注釈を解決する。
            /// `env` には impl の型パラメータ（`impl Opt T` の T 等）をスコープへ登録した環境を渡すことで、
            /// ジェネリック impl メソッドの引数注釈 `(x: T)` を TypeVar として解決できる。
            let resolveArgTypeWithSelfEnv (env: NameEnv) (selfType: TypeId) (arg: Ast.FnArg) =
                match arg with
                | :? Ast.FnArg.Inferred as inferredArg when inferredArg.name = "self" -> selfType
                | _ -> env.resolveArgType arg
            /// 既定の bootstrap 環境で引数型を解決する後方互換ヘルパ（role など型パラメータ非対応の文脈用）。
            let resolveArgTypeWithSelf (selfType: TypeId) (arg: Ast.FnArg) =
                resolveArgTypeWithSelfEnv bootstrapNameEnv selfType arg

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
                              unionInfo = None
                              methods = Map.empty }
                            defs)
                    Map.empty

            // union 宣言を abstract class（ルート型）+ 派生クラス（バリアント）群へ正規化する。
            // - ルート型: union 本体のフィールドを持つ abstract class（isAbstract=true）。
            // - 各バリアント: baseType=ルート型 の具象クラス。自身のフィールドのみを保持し、
            //   union フィールドは継承で共有する（メンバー解決が baseType 連鎖を辿る）。
            let dataTypeDefs =
                resolvedModule.unionDecls
                |> List.fold
                    (fun defs resolvedUnionDecl ->
                        // ネスト union を含む完全修飾名（例: "Color'HueColor"）をフィールド/型シンボル名の接頭辞に用いる。
                        let unionName = resolvedUnionDecl.qualifiedName
                        let unionSid = resolvedUnionDecl.typeSid
                        // ネスト union の場合は親 union を基底型とする（再帰的クラス階層）。
                        let unionBaseType = resolvedUnionDecl.baseUnionSid |> Option.map TypeId.Name

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

                        // ルート型（abstract class）を生成する。ネスト union は親 union を基底型に持つ。
                        let unionRootHirFields =
                            unionFieldDefs
                            |> List.map (fun fieldDef -> Hir.Field(fieldDef.sid, fieldDef.typ, Hir.Expr.Unit(fieldDef.span), fieldDef.span))
                        types.Add(Hir.Type(unionSid, false, true, unionBaseType, resolvedUnionDecl.typeParams, unionRootHirFields, []))

                        // 各バリアントの DataTypeDef を構築し、defs へ追加する。
                        // 本体内バリアント（decl.variants）に加え、extendable union の外部バリアント
                        // （externalVariants, union 本体外で宣言された struct/object）も同様に処理する。
                        let variantInfos, defsWithVariants =
                            (resolvedUnionDecl.decl.variants @ resolvedUnionDecl.externalVariants)
                            |> List.fold
                                (fun (variantAcc, defsAcc) variant ->
                                    // ネスト union バリアント（Ast.Decl.Union）は、それ自身が別の ResolvedUnionDecl として
                                    // 独立に型・メタデータを生成する。ここでは UnionVariantDef{isUnion=true} として記録するのみで、
                                    // Hir.Type や DataTypeDef の再生成は行わない。
                                    match variant with
                                    | :? Ast.Decl.Union as nestedUnion ->
                                        match resolvedUnionDecl.variantSids |> Map.tryFind nestedUnion.name with
                                        | Some nestedSid ->
                                            let variantInfo = { name = nestedUnion.name; typeSid = nestedSid; isUnion = true; objectFieldInits = None; span = variant.span }
                                            variantAcc @ [ variantInfo ], defsAcc
                                        | None -> variantAcc, defsAcc
                                    | _ ->
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
                                              unionInfo = None
                                              methods = Map.empty }
                                        let variantInfo = { name = variantName; typeSid = variantSid; isUnion = false; objectFieldInits = objectFieldInits; span = variant.span }
                                        variantAcc @ [ variantInfo ], Map.add qualifiedName variantDef defsAcc)
                                ([], defs)

                        // ルート型の DataTypeDef を登録する。ネスト union は親 union を基底型に持つため、
                        // collectInheritedFields が親 union の共有フィールドまで辿れるよう baseType を設定する。
                        Map.add
                            unionName
                            { typeSid = unionSid
                              baseType = unionBaseType
                              delegatedByFieldName = None
                              typeParams = resolvedUnionDecl.typeParams
                              fields = unionFieldDefs
                              hiddenFields = []
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

            // インポートした union のバリアント DataTypeDef を蓄積する。union ルートの再構築時に
            // 各バリアント（修飾名 `Union'Variant`）の def もここへ登録し、フォールド後に defs へマージする。
            let importedUnionVariantDefs = System.Collections.Generic.Dictionary<string, DataTypeDef>()

            let importedTypeDefs, importedTypeDiagnostics =
                allImportedTypeAliases
                |> Map.toList
                |> List.fold
                    (fun (defs, diagnostics) (aliasName, fullTypePath) ->
                        match availableTypeDecls.TryFind(fullTypePath) with
                        | None when importedDependencyTypeDefs.ContainsKey(fullTypePath) ->
                            let importedDef = importedDependencyTypeDefs[fullTypePath]
                            resolvedModule.moduleScope.DeclareType(aliasName, TypeId.Name importedDef.typeSid)
                            // union 型の場合、バリアント DataTypeDef も修飾名キーで登録しスコープへ宣言する。
                            // これにより `Union'Variant` 構築式・パターンが atlalib 経由で解決できる。
                            match importedDef.unionInfo with
                            | Some unionTypeDef ->
                                let lastDotInPath = fullTypePath.LastIndexOf('.')
                                let srcModuleName = if lastDotInPath > 0 then fullTypePath.Substring(0, lastDotInPath) else ""
                                let rec registerVariants (parentAliasName: string) (variants: UnionVariantDef list) =
                                    for variant in variants do
                                        let qualifiedVariantAlias = sprintf "%s'%s" parentAliasName variant.name
                                        let variantFullKey = $"{srcModuleName}.{qualifiedVariantAlias}"
                                        match importedDependencyTypeDefs.TryFind(variantFullKey) with
                                        | Some fullVariantDef ->
                                            importedUnionVariantDefs.[qualifiedVariantAlias] <- fullVariantDef
                                            resolvedModule.moduleScope.DeclareType(qualifiedVariantAlias, TypeId.Name fullVariantDef.typeSid)
                                            if variant.isUnion then
                                                match fullVariantDef.unionInfo with
                                                | Some nestedUnionInfo -> registerVariants qualifiedVariantAlias nestedUnionInfo.variants
                                                | None -> ()
                                        | None -> ()
                                registerVariants aliasName unionTypeDef.variants
                            | None -> ()
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
                                      unionInfo = None
                                      methods = Map.empty }
                                | :? Ast.Decl.Union as unionDecl ->
                                    // インポートした union を root + 各バリアント DataTypeDef へ再構築する。
                                    // バリアント型 SID は元モジュールのエクスポート（type:Union'Variant）を再利用し、
                                    // 同一 symbolTable 上で生成済みの CIL 型と一致させる。Phase 1 は単層 union を対象とする。
                                    let unionTypeParams = unionDecl.typeParams
                                    let unionRootSid = typeSid
                                    // union 本体フィールド（共有フィールド）を解決する。
                                    let fieldNameEnv =
                                        if unionTypeParams.IsEmpty then bootstrapNameEnv
                                        else
                                            let sub = bootstrapNameEnv.sub()
                                            for p in unionTypeParams do sub.scope.DeclareType(p, TypeId.TypeVar p)
                                            sub
                                    let resolveExportedSid (name: string) (typ: TypeId) =
                                        match sourceModuleExports |> Map.tryFind name with
                                        | Some exportInfo -> exportInfo.symbolId, exportInfo.typ
                                        | None ->
                                            let sid = symbolTable.NextId()
                                            symbolTable.Add(sid, { name = name; typ = typ; kind = SymbolKind.Local() })
                                            sid, typ
                                    let unionFieldDefs =
                                        unionDecl.fields
                                        |> List.choose (fun item ->
                                            match item with
                                            | :? Ast.DataItem.Field as fieldItem ->
                                                let fieldType = fieldNameEnv.resolveTypeExpr fieldItem.typeExpr
                                                let fieldSid, ft = resolveExportedSid $"field:{typeNameForLookup}.{fieldItem.name}" fieldType
                                                Some { name = fieldItem.name; sid = fieldSid; typ = ft; isMutable = fieldItem.isMutable; span = fieldItem.span }
                                            | _ -> None)
                                    if not isReusingExistingType then
                                        let rootFields = unionFieldDefs |> List.map (fun fd -> Hir.Field(fd.sid, fd.typ, Hir.Expr.Unit(fd.span), fd.span))
                                        types.Add(Hir.Type(unionRootSid, false, true, None, unionTypeParams, rootFields, []))
                                    // 各バリアントの型 SID と DataTypeDef を再構築する。ネスト union も再帰的に処理する。
                                    let rec processVariants (parentSid: SymbolId) (qualifiedParentName: string) (variants: Ast.Decl list) : UnionVariantDef list =
                                        variants
                                        |> List.choose (fun variant ->
                                            match variant with
                                            | :? Ast.Decl.Union as nestedUnionDecl ->
                                                // ネスト union バリアント: 抽象クラスとして再構築し、そのバリアントも再帰的に処理する。
                                                let nestedQualifiedName = sprintf "%s'%s" qualifiedParentName nestedUnionDecl.name
                                                let nestedSid =
                                                    match sourceModuleExports |> Map.tryFind $"type:{nestedQualifiedName}" with
                                                    | Some e -> e.symbolId
                                                    | None ->
                                                        let s = symbolTable.NextId()
                                                        symbolTable.Add(s, { name = nestedQualifiedName; typ = TypeId.Name s; kind = SymbolKind.Local() })
                                                        s
                                                resolvedModule.moduleScope.DeclareType(nestedQualifiedName, TypeId.Name nestedSid)
                                                let nestedFieldDefs =
                                                    nestedUnionDecl.fields
                                                    |> List.choose (fun item ->
                                                        match item with
                                                        | :? Ast.DataItem.Field as fieldItem ->
                                                            let fieldType = fieldNameEnv.resolveTypeExpr fieldItem.typeExpr
                                                            let fieldSid, ft = resolveExportedSid $"field:{nestedQualifiedName}.{fieldItem.name}" fieldType
                                                            Some { name = fieldItem.name; sid = fieldSid; typ = ft; isMutable = fieldItem.isMutable; span = fieldItem.span }
                                                        | _ -> None)
                                                if not isReusingExistingType then
                                                    let rootFields = nestedFieldDefs |> List.map (fun fd -> Hir.Field(fd.sid, fd.typ, Hir.Expr.Unit(fd.span), fd.span))
                                                    types.Add(Hir.Type(nestedSid, false, true, Some(TypeId.Name parentSid), unionTypeParams, rootFields, []))
                                                let nestedVariantInfos = processVariants nestedSid nestedQualifiedName nestedUnionDecl.variants
                                                let nestedUnionDef =
                                                    { typeSid = nestedSid
                                                      baseType = Some(TypeId.Name parentSid)
                                                      delegatedByFieldName = None
                                                      typeParams = unionTypeParams
                                                      fields = nestedFieldDefs
                                                      hiddenFields = []
                                                      unionInfo = Some { isExtendable = nestedUnionDecl.isExtendable; variants = nestedVariantInfos }
                                                      methods = Map.empty }
                                                importedUnionVariantDefs.[nestedQualifiedName] <- nestedUnionDef
                                                Some { name = nestedUnionDecl.name; typeSid = nestedSid; isUnion = true; objectFieldInits = None; span = nestedUnionDecl.span }
                                            | _ ->
                                                let variantNameOpt, ownItems, objInits =
                                                    match variant with
                                                    | :? Ast.Decl.Data as sv -> Some sv.name, sv.items, None
                                                    | :? Ast.Decl.Object as ov ->
                                                        let inits = ov.fieldInits |> List.choose (fun i -> match i with | :? Ast.DataInitField.Field as f -> Some(f.name, f.value) | _ -> None)
                                                        Some ov.name, [], Some inits
                                                    | _ -> None, [], None
                                                match variantNameOpt with
                                                | None -> None
                                                | Some vName ->
                                                    let qualifiedName = sprintf "%s'%s" qualifiedParentName vName
                                                    let variantSid =
                                                        match sourceModuleExports |> Map.tryFind $"type:{qualifiedName}" with
                                                        | Some e -> e.symbolId
                                                        | None ->
                                                            let s = symbolTable.NextId()
                                                            symbolTable.Add(s, { name = qualifiedName; typ = TypeId.Name s; kind = SymbolKind.Local() })
                                                            s
                                                    let ownFieldDefs =
                                                        ownItems
                                                        |> List.choose (fun item ->
                                                            match item with
                                                            | :? Ast.DataItem.Field as fieldItem ->
                                                                let fieldType = fieldNameEnv.resolveTypeExpr fieldItem.typeExpr
                                                                let fieldSid, ft = resolveExportedSid $"field:{qualifiedName}.{fieldItem.name}" fieldType
                                                                Some { name = fieldItem.name; sid = fieldSid; typ = ft; isMutable = fieldItem.isMutable; span = fieldItem.span }
                                                            | _ -> None)
                                                    if not isReusingExistingType then
                                                        let vFields = ownFieldDefs |> List.map (fun fd -> Hir.Field(fd.sid, fd.typ, Hir.Expr.Unit(fd.span), fd.span))
                                                        types.Add(Hir.Type(variantSid, false, false, Some(TypeId.Name parentSid), unionTypeParams, vFields, []))
                                                    let variantDef =
                                                        { typeSid = variantSid
                                                          baseType = Some(TypeId.Name parentSid)
                                                          delegatedByFieldName = None
                                                          typeParams = unionTypeParams
                                                          fields = ownFieldDefs
                                                          hiddenFields = []
                                                          unionInfo = None
                                                          methods = Map.empty }
                                                    importedUnionVariantDefs.[qualifiedName] <- variantDef
                                                    resolvedModule.moduleScope.DeclareType(qualifiedName, TypeId.Name variantSid)
                                                    Some { name = vName; typeSid = variantSid; isUnion = false; objectFieldInits = objInits; span = variant.span })
                                    let variantInfos = processVariants unionRootSid typeNameForLookup unionDecl.variants
                                    { typeSid = unionRootSid
                                      baseType = None
                                      delegatedByFieldName = None
                                      typeParams = unionTypeParams
                                      fields = unionFieldDefs
                                      hiddenFields = []
                                      unionInfo = Some { isExtendable = unionDecl.isExtendable; variants = variantInfos }
                                      methods = Map.empty }
                                | _ ->
                                    { typeSid = typeSid
                                      baseType = None
                                      delegatedByFieldName = None
                                      typeParams = []
                                      fields = []
                                      hiddenFields = []
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

                                // `struct T: NativeClass` パターンを確認する。
                                // モジュールエクスポートの "implBase:{TypeName}" キーが存在すれば native base あり。
                                let hasNativeBase = sourceModuleExports |> Map.containsKey $"implBase:{typeNameForLookup}"
                                if hasNativeBase then
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
            // インポート union のバリアント DataTypeDef を修飾名キーで合流する。
            let dataTypeDefs =
                importedUnionVariantDefs
                |> Seq.fold (fun defs (kvp: System.Collections.Generic.KeyValuePair<string, DataTypeDef>) -> Map.add kvp.Key kvp.Value defs) dataTypeDefs

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
                                                    |> List.map (resolveArgTypeWithSelfEnv implNameEnv (TypeId.Name dataTypeDef.typeSid))
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
                                                (methodSid, methodDecl, dataTypeDef.typeSid, isStatic, overrideTarget, implDecl.typeParams) :: declAcc,
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
            |> List.iter (fun (methodSid, methodDecl, targetTypeSid, isStatic, overrideTarget, implTypeParams) ->
                // impl の型パラメータ（`impl Opt T` の T 等）を本体解析用の nameEnv へ注入する。
                // これによりジェネリック union/struct のメソッド本体で戻り値・引数の `T` 注釈が解決できる。
                let methodNameEnv =
                    if List.isEmpty implTypeParams then nameEnv
                    else
                        let sub = nameEnv.sub()
                        for paramName in implTypeParams do
                            sub.scope.DeclareType(paramName, TypeId.TypeVar paramName)
                        sub
                let hirMethod = ExprAnalyze.analyzeMethodCoreWithOverride methodNameEnv typeEnv methodSid methodDecl overrideTarget
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
