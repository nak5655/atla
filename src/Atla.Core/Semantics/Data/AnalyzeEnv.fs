namespace Atla.Core.Semantics.Data

open Atla.Core.Syntax.Data

module AnalyzeEnv =

    type DataFieldDef =
        { name: string
          sid: SymbolId
          typ: TypeId
          span: Atla.Core.Data.Span }

    type DataTypeDef =
        { typeSid: SymbolId
          baseType: TypeId option
          delegatedByFieldName: string option
          fields: DataFieldDef list
          methods: Map<string, SymbolId * TypeId * bool> }

    type ModuleExport =
        { symbolId: SymbolId
          typ: TypeId }

    type NameEnv
        (
            symbolTable: SymbolTable,
            scope: Scope,
            dataTypeDefs: Map<string, DataTypeDef>,
            importedModuleExports: Map<string, Map<string, ModuleExport>>
        ) =
        member this.symbolTable = symbolTable
        member this.scope = scope
        member this.dataTypeDefs = dataTypeDefs
        member this.importedModuleExports = importedModuleExports

        // TypeExprをTypeIdへ解決する。
        member this.resolveTypeExpr (typeExpr: Ast.TypeExpr) : TypeId =
            let resolveNamedType (name: string) (span: Atla.Core.Data.Span) : TypeId =
                match scope.ResolveType(name) with
                | Some typ -> typ
                | _ -> TypeId.Error (sprintf "Undefined type '%s'" name)

            match typeExpr with
            | :? Ast.TypeExpr.Unit -> TypeId.Unit
            | :? Ast.TypeExpr.Id as idTypeExpr ->
                resolveNamedType idTypeExpr.name idTypeExpr.span
            | :? Ast.TypeExpr.Apply as applyTypeExpr ->
                let resolveArgs () =
                    let resolvedArgs = applyTypeExpr.args |> List.map this.resolveTypeExpr
                    let firstArgError =
                        resolvedArgs
                        |> List.tryPick (fun argType ->
                            match argType with
                            | TypeId.Error message -> Some message
                            | _ -> None)
                    resolvedArgs, firstArgError

                match applyTypeExpr.head with
                | :? Ast.TypeExpr.Id as headId when headId.name = "Array" ->
                    let resolvedArgs, firstArgError = resolveArgs ()
                    match firstArgError, resolvedArgs with
                    | Some message, _ -> TypeId.Error message
                    | None, [elemType] -> TypeId.App(TypeId.Native typeof<System.Array>, [ elemType ])
                    | None, _ -> TypeId.Error("Array type expects exactly one type argument")
                | _ ->
                    let resolvedHead = this.resolveTypeExpr applyTypeExpr.head
                    let resolvedArgs, firstArgError = resolveArgs ()
                    match resolvedHead, firstArgError with
                    | TypeId.Error message, _ -> TypeId.Error message
                    | _, Some message -> TypeId.Error message
                    | _, None -> TypeId.App(resolvedHead, resolvedArgs)
            // 関数型（例: Int -> Int, () -> ()）を TypeId.Fn に変換する。
            // () -> T はユニット引数構文のため、0引数関数 Fn([], T) に解決する。
            // これにより fn () -> expr（0引数ラムダ）および fn foo (): T（0引数メソッド）と整合する。
            | :? Ast.TypeExpr.Arrow as arrowTypeExpr ->
                let argType = this.resolveTypeExpr arrowTypeExpr.arg
                let retType = this.resolveTypeExpr arrowTypeExpr.ret
                match argType with
                | TypeId.Unit -> TypeId.Fn([], retType)
                | _ -> TypeId.Fn([ argType ], retType)
            | _ -> TypeId.Error "Unsupported type expression type"

        member this.resolveArgType (arg: Ast.FnArg) : TypeId =
            match arg with
            | :? Ast.FnArg.Named as namedArg -> this.resolveTypeExpr namedArg.typeExpr
            | :? Ast.FnArg.Unit -> TypeId.Unit
            | _ -> TypeId.Error "Unsupported function argument type"

        member this.declareLocal (name: string) (tid: TypeId) : SymbolId =
            let sid = symbolTable.NextId()
            let symInfo = { name = name; typ = tid; kind = SymbolKind.Local() }
            symbolTable.Add(sid, symInfo)
            scope.DeclareVar(name, sid)
            sid

        member this.declareArg (name: string) (tid: TypeId) : SymbolId =
            let sid = symbolTable.NextId()
            let symInfo = { name = name; typ = tid; kind = SymbolKind.Arg() }
            symbolTable.Add(sid, symInfo)
            scope.DeclareVar(name, sid)
            sid

        member this.resolveVar (name: string) : SymbolId list =
            scope.ResolveVar(name)

        member this.resolveSymType (sid: SymbolId) : TypeId =
            match symbolTable.Get(sid) with
            | Some(symInfo) -> symInfo.typ
            | _ -> TypeId.Error (sprintf "Undefined symbol '%A'" sid)

        member this.resolveSym(sid: SymbolId) : SymbolInfo option =
            symbolTable.Get(sid)

        member this.sub(): NameEnv =
            let blockScope = Scope(Some this.scope)
            NameEnv(this.symbolTable, blockScope, this.dataTypeDefs, this.importedModuleExports)

        /// import された Atla モジュールからメンバー参照を解決する。
        member this.tryResolveImportedModuleMember (moduleAlias: string) (memberName: string) : ModuleExport option =
            match this.importedModuleExports.TryFind(moduleAlias) with
            | Some members -> members.TryFind(memberName)
            | None -> None

        /// 継承チェーンを辿り、`actualTypeSid <: expectedTypeSid` が成立するかを判定する。
        /// Atla 型の `TypeId.Name` チェーンのみを辿る。`TypeId.Native`（.NET 継承）は終端とみなす。
        member this.isSubtype (actualTypeSid: SymbolId) (expectedTypeSid: SymbolId) : bool =
            let bySid =
                this.dataTypeDefs
                |> Map.toSeq
                |> Seq.map (fun (_, def) -> def.typeSid, def)
                |> Map.ofSeq

            let rec loop (currentSid: SymbolId) (visited: Set<int>) =
                if currentSid.id = expectedTypeSid.id then
                    true
                elif visited |> Set.contains currentSid.id then
                    false
                else
                    match bySid |> Map.tryFind currentSid with
                    | Some currentDef ->
                        match currentDef.baseType with
                        | Some (TypeId.Name baseSid) -> loop baseSid (visited |> Set.add currentSid.id)
                        | _ -> false
                    | None -> false

            loop actualTypeSid Set.empty

    type TypeEnv(typSubst: TypeSubst, metaFactory: TypeMetaFactory) =
        member this.typSubst = typSubst

        member this.unifyTypes (tid1: TypeId) (tid2: TypeId) : Result<unit, UnifyError> =
            Type.unify typSubst tid1 tid2 |> Result.map ignore

        member this.resolveType (tid: TypeId) : TypeId =
            Type.resolve typSubst tid

        member this.canUnify (tid1: TypeId) (tid2: TypeId) : bool =
            Type.canUnify typSubst tid1 tid2

        member this.freshMeta() : TypeId =
            TypeId.Meta(metaFactory.Fresh())
