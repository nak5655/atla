namespace Atla.Compiler.Semantics

open System.Reflection
open System.Collections.Generic
open Atla.Compiler.Syntax.Data
open Atla.Compiler.Semantics.Data

module Analyze =
    type NameEnv(symbolTable: SymbolTable, scope: Scope) =
        member this.symbolTable = symbolTable
        member this.scope = scope

        member this.resolveTypeExpr (typeExpr: Ast.TypeExpr) : TypeId =
            match typeExpr with
            | :? Ast.TypeExpr.Unit -> TypeId.Unit
            | :? Ast.TypeExpr.Id as idTypeExpr ->
                match scope.ResolveType(idTypeExpr.name) with
                | Some t -> t
                | _ -> TypeId.Error (sprintf "Undefined type '%s' at %A" idTypeExpr.name idTypeExpr.span)
            | _ -> failwith "Unsupported type expression type"

        member this.resolveArgType (arg: Ast.FnArg) : TypeId =
            match arg with
            | :? Ast.FnArg.Named as namedArg -> this.resolveTypeExpr namedArg.typeExpr
            | :? Ast.FnArg.Unit -> TypeId.Unit
            | _ -> failwith "Unsupported function argument type"

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

        member this.resolveVar (name: string) (tid: TypeId) : SymbolId list =
            scope.ResolveVar(name, tid)

        member this.resolveSymType (sid: SymbolId) : TypeId =
            match symbolTable.Get(sid) with
            | Some(symInfo) -> symInfo.typ
            | _ -> TypeId.Error (sprintf "Undefined symbol '%A'" sid)

        member this.resolveSym(sid: SymbolId) : SymbolInfo option =
            symbolTable.Get(sid)

        member this.sub(): NameEnv =
            let blockScope = Scope(Some this.scope)
            NameEnv(this.symbolTable, blockScope)

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

    let private resolveTyp (nameEnv: NameEnv) (typeEnv: TypeEnv) (tid: TypeId) : SymbolInfo option =
        match typeEnv.resolveType tid with
        | TypeId.Name sid -> nameEnv.resolveSym sid
        | _ -> None

    let private resolveNativeMember (typeEnv: TypeEnv) (memberInfos: MemberInfo list) (tid: TypeId) : (MemberInfo * TypeId) list =
        let result = List<MemberInfo * TypeId>()
        for memberInfo in memberInfos do
            match memberInfo with
            | :? MethodInfo as methodInfo ->
                let methodType = TypeId.Fn([for p in methodInfo.GetParameters() -> TypeId.fromSystemType p.ParameterType], TypeId.fromSystemType methodInfo.ReturnType)
                if typeEnv.canUnify methodType tid then
                    result.Add(memberInfo, methodType)
            | :? FieldInfo as fieldInfo ->
                let fieldType = TypeId.fromSystemType fieldInfo.FieldType
                if typeEnv.canUnify fieldType tid then
                    result.Add(memberInfo, fieldType)
            | :? PropertyInfo as propertyInfo ->
                let propertyType = TypeId.fromSystemType propertyInfo.PropertyType
                if typeEnv.canUnify propertyType tid then
                    result.Add(memberInfo, propertyType)
            | _ -> ()
        Seq.toList result

    let private errorExpr (tid: TypeId) (span: Atla.Compiler.Data.Span) (message: string) : Hir.Expr =
        Hir.Expr.ExprError(message, tid, span)

    let private resultToExpr (tid: TypeId) (span: Atla.Compiler.Data.Span) (resolved: Result<Hir.Expr, string>) : Hir.Expr =
        match resolved with
        | Result.Ok expr -> expr
        | Result.Error message -> errorExpr tid span message

    let private unifyOrError (typeEnv: TypeEnv) (expected: TypeId) (actual: TypeId) (span: Atla.Compiler.Data.Span) : Result<unit, Hir.Expr> =
        match typeEnv.unifyTypes expected actual with
        | Result.Ok _ -> Result.Ok ()
        | Result.Error err -> Result.Error(errorExpr expected span (UnifyError.toMessage err))

    let rec private analyzeExprAsCallable (nameEnv: NameEnv) (typeEnv: TypeEnv) (expr: Ast.Expr) (tid: TypeId): Hir.Callable option =
        match analyzeExpr nameEnv typeEnv expr tid with
        | Hir.Expr.Id (sid, _, _) ->
            match nameEnv.resolveSym sid with
            | Some symInfo ->
                match symInfo.kind with
                | SymbolKind.External(ExternalBinding.NativeMethodGroup methodInfos) -> Some (Hir.Callable.NativeMethodGroup methodInfos)
                | SymbolKind.External(ExternalBinding.ConstructorGroup ctorInfos) -> Some (Hir.Callable.NativeConstructorGroup ctorInfos)
                | SymbolKind.BuiltinOperator op -> Some (Hir.Callable.BuiltinOperator op)
                | SymbolKind.Local ()
                | SymbolKind.Arg () ->
                    match typeEnv.resolveType symInfo.typ with
                    | TypeId.Fn _ -> Some (Hir.Callable.Fn sid)
                    | _ -> None
                | _ -> None
            | None -> None
        | Hir.Expr.MemberAccess (memberInfo, _, _, _) ->
            match memberInfo with
            | Hir.Member.NativeMethod methodInfo -> Some (Hir.Callable.NativeMethod methodInfo)
            | _ -> None
        | _ -> None

    and private analyzeExpr (nameEnv: NameEnv) (typeEnv: TypeEnv) (expr: Ast.Expr) (tid: TypeId) : Hir.Expr =
        match expr with
        | :? Ast.Expr.Unit as unitExpr ->
            match unifyOrError typeEnv tid TypeId.Unit unitExpr.span with
            | Result.Ok _ -> Hir.Expr.Unit(unitExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.Int as intExpr ->
            match unifyOrError typeEnv tid TypeId.Int intExpr.span with
            | Result.Ok _ -> Hir.Expr.Int(intExpr.value, intExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.Float as floatExpr ->
            match unifyOrError typeEnv tid TypeId.Float floatExpr.span with
            | Result.Ok _ -> Hir.Expr.Float(floatExpr.value, floatExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.String as stringExpr ->
            match unifyOrError typeEnv tid TypeId.String stringExpr.span with
            | Result.Ok _ -> Hir.Expr.String(stringExpr.value, stringExpr.span)
            | Result.Error exprErr -> exprErr
        | :? Ast.Expr.Id as idExpr ->
            match nameEnv.resolveVar idExpr.name tid with
            | [sid] ->
                match unifyOrError typeEnv tid (nameEnv.resolveSymType sid) idExpr.span with
                | Result.Ok _ -> Hir.Expr.Id(sid, nameEnv.resolveSymType sid, idExpr.span)
                | Result.Error exprErr -> exprErr
            | [] -> Hir.Expr.ExprError(sprintf "Undefined variable '%s' at %A" idExpr.name idExpr.span, tid, idExpr.span)
            | _ -> Hir.Expr.ExprError(sprintf "Ambiguous variable '%s' at %A" idExpr.name idExpr.span, tid, idExpr.span)
        | :? Ast.Expr.Block as blockExpr ->
            let blockNameEnv = nameEnv.sub()
            let stmts = blockExpr.stmts |> List.map (analyzeStmt blockNameEnv typeEnv)
            match List.last stmts with
            | Hir.Stmt.ExprStmt (expr, _) ->
                match unifyOrError typeEnv tid expr.typ blockExpr.span with
                | Result.Ok _ ->
                    let leadingStmts = stmts |> List.take (stmts.Length - 1)
                    Hir.Expr.Block(leadingStmts, expr, tid, blockExpr.span)
                | Result.Error exprErr -> exprErr
            | _ ->
                let unitExpr = Hir.Expr.Unit ({ left = blockExpr.span.right; right = blockExpr.span.right })
                Hir.Expr.Block(stmts, unitExpr, tid, blockExpr.span)
        | :? Ast.Expr.Apply as applyExpr ->
            let args = applyExpr.args |> List.map (fun arg -> analyzeExpr nameEnv typeEnv arg (typeEnv.freshMeta()))
            let funcType = args |> List.map (fun arg -> arg.typ) |> fun argTypes -> TypeId.Fn(argTypes, tid)
            let callable = analyzeExprAsCallable nameEnv typeEnv applyExpr.func funcType
            match callable with
            | Some resolvedCallable -> Hir.Expr.Call(resolvedCallable, None, args, tid, applyExpr.span)
            | None ->
                match args with
                | [Hir.Expr.Unit _] ->
                    let zeroArgFuncType = TypeId.Fn([], tid)
                    match analyzeExprAsCallable nameEnv typeEnv applyExpr.func zeroArgFuncType with
                    | Some resolvedCallable -> Hir.Expr.Call(resolvedCallable, None, [], tid, applyExpr.span)
                    | None -> Hir.Expr.ExprError(sprintf "Expression is not callable at %A" applyExpr.span, tid, applyExpr.span)
                | _ -> Hir.Expr.ExprError(sprintf "Expression is not callable at %A" applyExpr.span, tid, applyExpr.span)
        | :? Ast.Expr.MemberAccess as memberAccessExpr ->
            let resolveMemberFromSystemTypeResult (sysType: System.Type) : Result<Hir.Expr, string> =
                let memberInfos =
                    sysType.GetMembers(BindingFlags.Public ||| BindingFlags.Static)
                    |> Seq.filter (fun m -> m.Name = memberAccessExpr.memberName)
                    |> Seq.toList

                match resolveNativeMember typeEnv memberInfos tid with
                | [memberInfo, resolvedTid] ->
                    match memberInfo with
                    | :? MethodInfo as methodInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, None, resolvedTid, memberAccessExpr.span))
                    | :? FieldInfo as fieldInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeField fieldInfo, None, resolvedTid, memberAccessExpr.span))
                    | :? PropertyInfo as propertyInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeProperty propertyInfo, None, resolvedTid, memberAccessExpr.span))
                    | _ -> Result.Error (sprintf "Unsupported member type for '%s' at %A" memberAccessExpr.memberName memberAccessExpr.span)
                | [] -> Result.Error (sprintf "Undefined member '%s' for type '%A' at %A" memberAccessExpr.memberName sysType memberAccessExpr.span)
                | _ -> Result.Error (sprintf "Ambiguous member '%s' for type '%A' at %A" memberAccessExpr.memberName sysType memberAccessExpr.span)

            let resolveFromSymInfo (typeName: obj) (symInfo: SymbolInfo) : Result<Hir.Expr, string> =
                match symInfo.kind with
                | SymbolKind.External(ExternalBinding.SystemTypeRef sysType) when not (obj.ReferenceEquals(sysType, null)) ->
                    resolveMemberFromSystemTypeResult sysType
                | SymbolKind.External(ExternalBinding.SystemTypeRef _) ->
                    Result.Error (sprintf "System type '%A' could not be loaded at %A" typeName memberAccessExpr.span)
                | _ ->
                    Result.Error (sprintf "Type '%A' does not support member access at %A" symInfo.typ memberAccessExpr.span)

            let resolvedMemberResult =
                match memberAccessExpr.receiver with
                | :? Ast.Expr.Id as receiverId ->
                    match nameEnv.scope.ResolveType(receiverId.name) with
                    | Some (TypeId.Name sid) ->
                        match nameEnv.resolveSym sid with
                        | Some symInfo ->
                            match symInfo.kind with
                            | SymbolKind.External(ExternalBinding.SystemTypeRef sysType) when not (obj.ReferenceEquals(sysType, null)) -> resolveMemberFromSystemTypeResult sysType
                            | SymbolKind.External(ExternalBinding.SystemTypeRef _) -> Result.Error (sprintf "System type '%s' could not be loaded at %A" receiverId.name memberAccessExpr.span)
                            | _ -> Result.Error (sprintf "Type '%s' is not a system type at %A" receiverId.name memberAccessExpr.span)
                        | None -> Result.Error (sprintf "Undefined type symbol '%s' at %A" receiverId.name memberAccessExpr.span)
                    | _ ->
                        let receiver = analyzeExpr nameEnv typeEnv memberAccessExpr.receiver (typeEnv.freshMeta())
                        match resolveTyp nameEnv typeEnv receiver.typ with
                        | Some symInfo -> resolveFromSymInfo symInfo.name symInfo
                        | None -> Result.Error (sprintf "Undefined type '%A' at %A" receiver.typ memberAccessExpr.span)
                | _ ->
                    let receiver = analyzeExpr nameEnv typeEnv memberAccessExpr.receiver (typeEnv.freshMeta())
                    match resolveTyp nameEnv typeEnv receiver.typ with
                    | Some symInfo -> resolveFromSymInfo symInfo.name symInfo
                    | None -> Result.Error (sprintf "Undefined type '%A' at %A" receiver.typ memberAccessExpr.span)

            resultToExpr tid memberAccessExpr.span resolvedMemberResult
        | :? Ast.Expr.StaticAccess as staticAccessExpr ->
            let resolvedStaticResult =
                match nameEnv.scope.ResolveType(staticAccessExpr.typeName) with
                | Some(TypeId.Name sid) ->
                    match nameEnv.resolveSym sid with
                    | Some symInfo ->
                        match symInfo.kind with
                        | SymbolKind.External(ExternalBinding.SystemTypeRef sysType) ->
                            if obj.ReferenceEquals(sysType, null) then
                                Result.Error (sprintf "System type '%s' could not be loaded at %A" staticAccessExpr.typeName staticAccessExpr.span)
                            else
                                let memberInfos =
                                    sysType.GetMembers(BindingFlags.Public ||| BindingFlags.Static)
                                    |> Seq.filter (fun m -> m.Name = staticAccessExpr.memberName)
                                    |> Seq.toList
                                match resolveNativeMember typeEnv memberInfos tid with
                                | [memberInfo, memberType] ->
                                    match memberInfo with
                                    | :? MethodInfo as methodInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, None, memberType, staticAccessExpr.span))
                                    | :? FieldInfo as fieldInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeField fieldInfo, None, memberType, staticAccessExpr.span))
                                    | :? PropertyInfo as propertyInfo -> Result.Ok (Hir.Expr.MemberAccess(Hir.Member.NativeProperty propertyInfo, None, memberType, staticAccessExpr.span))
                                    | _ -> Result.Error (sprintf "Unsupported member type for '%s.%s' at %A" staticAccessExpr.typeName staticAccessExpr.memberName staticAccessExpr.span)
                                | [] -> Result.Error (sprintf "Undefined member '%s' for type '%s' at %A" staticAccessExpr.memberName staticAccessExpr.typeName staticAccessExpr.span)
                                | _ -> Result.Error (sprintf "Ambiguous member '%s' for type '%s' at %A" staticAccessExpr.memberName staticAccessExpr.typeName staticAccessExpr.span)
                        | _ -> Result.Error (sprintf "Type '%s' is not a system type at %A" staticAccessExpr.typeName staticAccessExpr.span)
                    | None -> Result.Error (sprintf "Undefined type symbol '%s' at %A" staticAccessExpr.typeName staticAccessExpr.span)
                | Some _ -> Result.Error (sprintf "Unsupported type id for '%s' at %A" staticAccessExpr.typeName staticAccessExpr.span)
                | None -> Result.Error (sprintf "Undefined type '%s' at %A" staticAccessExpr.typeName staticAccessExpr.span)

            resultToExpr tid staticAccessExpr.span resolvedStaticResult
        | :? Ast.Expr.If as ifExpr ->
            let rec analyzeIfBranches (branches: Ast.IfBranch list) : Hir.Expr =
                match List.head branches with
                | :? Ast.IfBranch.Then as thenBranch ->
                    let cond = analyzeExpr nameEnv typeEnv thenBranch.cond TypeId.Bool
                    let body = analyzeExpr nameEnv typeEnv thenBranch.body tid
                    Hir.Expr.If(cond, body, analyzeIfBranches (List.tail branches), tid, { left = thenBranch.span.left; right = (List.last branches).span.right })
                | :? Ast.IfBranch.Else as elseBranch -> analyzeExpr nameEnv typeEnv elseBranch.body tid
                | _ -> failwith "Unsupported if branch type"
            analyzeIfBranches ifExpr.branches
        | _ -> failwith "Unsupported expression type"

    and private analyzeStmt (nameEnv: NameEnv) (typeEnv: TypeEnv) (stmt: Ast.Stmt) : Hir.Stmt =
        match stmt with
        | :? Ast.Stmt.Let as letStmt ->
            let tid = typeEnv.freshMeta ()
            let rhs = analyzeExpr nameEnv typeEnv letStmt.value tid
            let sid = nameEnv.declareLocal letStmt.name tid
            Hir.Stmt.Let(sid, false, rhs, letStmt.span)
        | :? Ast.Stmt.Var as varStmt ->
            let tid = typeEnv.freshMeta ()
            let rhs = analyzeExpr nameEnv typeEnv varStmt.value tid
            let sid = nameEnv.declareLocal varStmt.name tid
            Hir.Stmt.Let(sid, true, rhs, varStmt.span)
        | :? Ast.Stmt.Assign as assignStmt ->
            let tid = typeEnv.freshMeta ()
            let rhs = analyzeExpr nameEnv typeEnv assignStmt.value tid
            match nameEnv.resolveVar assignStmt.name rhs.typ with
            | [sid] -> Hir.Stmt.Assign(sid, rhs, assignStmt.span)
            | [] -> Hir.Stmt.ExprStmt(Hir.Expr.ExprError(sprintf "Undefined variable '%s' at %A" assignStmt.name assignStmt.span, TypeId.Error (sprintf "Undefined variable '%s'" assignStmt.name), assignStmt.span), assignStmt.span)
            | _ -> failwith (sprintf "Ambiguous variable '%s' at %A" assignStmt.name assignStmt.span)
        | :? Ast.Stmt.ExprStmt as exprStmt ->
            let expr = analyzeExpr nameEnv typeEnv exprStmt.expr (typeEnv.freshMeta ())
            Hir.Stmt.ExprStmt(expr, exprStmt.span)
        | :? Ast.Stmt.For as forStmt ->
            Hir.Stmt.ExprStmt(Hir.Expr.Unit(forStmt.span), forStmt.span)
        | _ -> failwith "Unsupported statement type"

    let private analyzeMethod (nameEnv: NameEnv) (typeEnv: TypeEnv) (fnDecl: Ast.Decl.Fn) : Hir.Method =
        let bodyNameEnv = nameEnv.sub()
        let retType = nameEnv.resolveTypeExpr fnDecl.ret
        let argTypes = fnDecl.args |> List.map bodyNameEnv.resolveArgType

        fnDecl.args
        |> List.iteri (fun index arg ->
            match arg with
            | :? Ast.FnArg.Named as namedArg ->
                let argType = argTypes.[index]
                bodyNameEnv.declareArg namedArg.name argType |> ignore
            | :? Ast.FnArg.Unit -> ()
            | _ -> failwith "Unsupported function argument type")

        let tid = TypeId.Fn(argTypes, retType)
        let sid = nameEnv.declareLocal fnDecl.name tid
        let body = analyzeExpr bodyNameEnv typeEnv fnDecl.body retType

        Hir.Method(sid, body, tid, fnDecl.span)

    let analyzeModule (symbolTable: SymbolTable, typeSubst: TypeSubst, moduleName: string, moduleAst: Ast.Module) : Result<Hir.Module, Error list> =
        let resolvedModule = Resolve.resolveModule (symbolTable, moduleName, moduleAst)
        let nameEnv = NameEnv(symbolTable, resolvedModule.moduleScope)
        let typeEnv = TypeEnv(typeSubst, TypeMetaFactory())

        let fields = List<Hir.Field>()
        let methods = List<Hir.Method>()
        let types = List<Hir.Type>()

        resolvedModule.fnDecls
        |> List.iter (fun fnDecl -> methods.Add(analyzeMethod nameEnv typeEnv fnDecl))

        let untypedHirModule =
            Hir.Module(
                resolvedModule.moduleName,
                types |> Seq.toList,
                fields |> Seq.toList,
                methods |> Seq.toList,
                resolvedModule.moduleScope
            )

        Infer.inferModule (typeSubst, untypedHirModule)
