namespace Atla.Compiler.Semantics

open System.Reflection
open System.Collections.Generic
open Atla.Compiler.Syntax.Data
open Atla.Compiler.Semantics.Data

module Analyze =
    type Env(symbolTable: SymbolTable, typSubst: TypeSubst, scope: Scope) =
        member this.symbolTable = symbolTable
        member this.typSubst = typSubst
        member this.scope = scope
        
        // 型式の名前解決
        member this.resolveTypeExpr (typeExpr: Ast.TypeExpr) : TypeId =
            match typeExpr with
            | :? Ast.TypeExpr.Unit -> TypeId.Unit
            | :? Ast.TypeExpr.Id as idTypeExpr ->
                match scope.ResolveType(idTypeExpr.name) with
                | Some t -> t
                | _ -> TypeId.Error (sprintf "Undefined type '%s' at %A" idTypeExpr.name idTypeExpr.span)
            | _ -> failwith "Unsupported type expression type"

        // 関数引数の型を解決する
        member this.resolveArgType (arg: Ast.FnArg) : TypeId =
            match arg with
            | :? Ast.FnArg.Named as namedArg -> this.resolveTypeExpr namedArg.typeExpr
            | :? Ast.FnArg.Unit -> TypeId.Unit
            | _ -> failwith "Unsupported function argument type"

        member this.declareLocal (name: string) (tid: TypeId) : SymbolId =
            let sid = symbolTable.NextId()
            let symInfo = SymbolInfo(name, tid, SymbolKind.Local())
            symbolTable.Add(sid, symInfo)
            scope.DeclareVar(name, sid)
            sid

        // シンボルテーブルにローカル変数を追加し、スコープに宣言する
        member this.declareLocalMeta (name: string) : SymbolId =
            let sid = symbolTable.NextId()
            let symInfo = SymbolInfo(name, TypeId.Meta (TypeMeta.fresh ()), SymbolKind.Local())
            symbolTable.Add(sid, symInfo)
            scope.DeclareVar(name, sid)
            sid
            
        member this.declareArg (name: string) (tid: TypeId) : SymbolId =
            let sid = symbolTable.NextId()
            let symInfo = SymbolInfo(name, tid, SymbolKind.Arg())
            symbolTable.Add(sid, symInfo)
            scope.DeclareVar(name, sid)
            sid

        member this.resolveVar (name: string) (tid: TypeId) : SymbolId list =
            scope.ResolveVar(name, tid)

        member this.declareSystemType (classPath: string) : SymbolId =
            let sid = symbolTable.NextId()
            let name = Array.last (classPath.Split('.'))
            let kind = SymbolKind.SystemType(System.Type.GetType(classPath))
            let symInfo = SymbolInfo(name, TypeId.Name sid, kind)
            symbolTable.Add(sid, symInfo)
            scope.DeclareType(name, TypeId.Name sid)
            sid

        member this.resolveSymType (sid: SymbolId) : TypeId =
            match symbolTable.Get(sid) with
            | Some(symInfo) -> symInfo.typ
            | _ -> TypeId.Error (sprintf "Undefined symbol '%A'" sid)

        member this.resolveSym(sid: SymbolId) : SymbolInfo option =
            symbolTable.Get(sid)

        member this.unifyTypes (tid1: TypeId) (tid2: TypeId) : unit =
            Type.unify typSubst tid1 tid2 |> ignore

        member this.resolveTyp (tid: TypeId) : SymbolInfo option =
            match Type.resolve typSubst tid with
            | TypeId.Name sid ->
                match symbolTable.Get(sid) with
                | Some symInfo -> Some symInfo
                | None -> None
            | _ -> None

        member this.resolveNativeMember (memberInfos: MemberInfo list) (tid: TypeId) : (MemberInfo * TypeId) list =
            let result = List<MemberInfo * TypeId>()
            for memberInfo in memberInfos do
                match memberInfo with
                | :? MethodInfo as methodInfo ->
                    let methodType = TypeId.Fn([for p in methodInfo.GetParameters() -> TypeId.fromSystemType p.ParameterType], TypeId.fromSystemType methodInfo.ReturnType)
                    if Type.canUnify typSubst methodType tid then
                        result.Add(memberInfo, methodType)
                | :? FieldInfo as fieldInfo ->
                    let fieldType = TypeId.fromSystemType fieldInfo.FieldType
                    if Type.canUnify typSubst fieldType tid then
                        result.Add(memberInfo, fieldType)
                | :? PropertyInfo as propertyInfo ->
                    let propertyType = TypeId.fromSystemType propertyInfo.PropertyType
                    if Type.canUnify typSubst propertyType tid then
                        result.Add(memberInfo, propertyType)
                | _ -> ()
            
            Seq.toList result

        member this.sub(): Env =
            let blockScope = Scope(Some this.scope)
            Env(this.symbolTable, this.typSubst, blockScope)

    // 式を名前解決したあと、関数として意味解析を行う
    let rec analyzeExprAsCallable (env:Env) (expr: Ast.Expr) (tid: TypeId): Hir.Callable option =
        match analyzeExpr env expr tid with
        | Hir.Expr.Id (sid, _tid, _) ->
            match env.resolveSym sid with
            | Some symInfo ->
                match symInfo.kind with
                | SymbolKind.NativeMethod methodInfos -> Some (Hir.Callable.NativeMethodGroup methodInfos)
                | SymbolKind.Constructor ctorInfos -> Some (Hir.Callable.NativeConstructorGroup ctorInfos)
                | SymbolKind.BuiltinOperator op -> Some (Hir.Callable.BuiltinOperator op)
                | SymbolKind.Local ()
                | SymbolKind.Arg () ->
                    match Type.resolve env.typSubst symInfo.typ with
                    | TypeId.Fn _ -> Some (Hir.Callable.Fn sid)
                    | _ -> None
                | _ -> None
            | None -> None
        | Hir.Expr.MemberAccess (memberInfo, _, _, _) ->
            match memberInfo with
            | Hir.Member.NativeMethod methodInfo -> Some (Hir.Callable.NativeMethod methodInfo)
            | _ -> None
        | _ -> None

    and analyzeExpr (env: Env) (expr: Ast.Expr) (tid: TypeId) : Hir.Expr =
        match expr with
        | :? Ast.Expr.Unit as unitExpr ->
            env.unifyTypes tid TypeId.Unit
            Hir.Expr.Unit(unitExpr.span)
        | :? Ast.Expr.Int as intExpr ->
            env.unifyTypes tid TypeId.Int
            Hir.Expr.Int(intExpr.value, intExpr.span)
        | :? Ast.Expr.Float as floatExpr ->
            env.unifyTypes tid TypeId.Float
            Hir.Expr.Float(floatExpr.value, floatExpr.span)
        | :? Ast.Expr.String as stringExpr ->
            env.unifyTypes tid TypeId.String
            Hir.Expr.String(stringExpr.value, stringExpr.span)
        | :? Ast.Expr.Id as idExpr ->
            match env.resolveVar idExpr.name tid with
            | [sid] ->
                env.unifyTypes tid (env.resolveSymType sid)
                Hir.Expr.Id(sid, env.resolveSymType sid, idExpr.span)
            | [] -> Hir.Expr.ExprError(sprintf "Undefined variable '%s' at %A" idExpr.name idExpr.span, tid, idExpr.span)
            | _ -> Hir.Expr.ExprError(sprintf "Ambiguous variable '%s' at %A" idExpr.name idExpr.span, tid, idExpr.span)
        | :? Ast.Expr.Block as blockExpr ->
            let blockEnv = env.sub()
            let stmts = blockExpr.stmts |> List.map (analyzeStmt blockEnv)
            match List.last stmts with
            | Hir.Stmt.ExprStmt (expr, _) ->
                env.unifyTypes tid expr.typ
                Hir.Expr.Block(stmts, expr, tid, blockExpr.span)
            | _ ->
                // ブロックの最後が式でない場合は、ブロック全体の値はUnitとする
                let unitExpr = Hir.Expr.Unit ({left = blockExpr.span.right; right = blockExpr.span.right})
                Hir.Expr.Block(stmts, unitExpr, tid, blockExpr.span)
        | :? Ast.Expr.Apply as applyExpr ->
            // 引数の名前解析と型推論を行いながら、Applyに畳み込む
            let args = applyExpr.args |> List.map (fun arg ->
                analyzeExpr env arg (TypeId.freshMeta()))
            // 引数の型から、関数本体の名前解析と型推論を行う
            let funcType = args |> List.map (fun arg -> arg.typ) |> fun argTypes -> TypeId.Fn(argTypes, tid)
            let callable = analyzeExprAsCallable env applyExpr.func funcType
            match callable with
            | Some callable ->
                Hir.Expr.Call(callable, None, args, tid, applyExpr.span)
            | _ -> Hir.Expr.ExprError(sprintf "Expression is not callable at %A" applyExpr.span, tid, applyExpr.span)
        | :? Ast.Expr.MemberAccess as memberAccessExpr ->
            // まずレシーバーの式を解析して型を推論する
            let receiver = analyzeExpr env memberAccessExpr.receiver (TypeId.freshMeta())
            // レシーバーの型から、メンバーアクセスの名前解決と型推論を行う
            match env.resolveTyp receiver.typ with
            | Some symInfo ->
                match symInfo.kind with
                | SymbolKind.SystemType sysType ->
                    // System.Typeのstaticメンバーアクセスをサポートする
                    let methodInfos = sysType.GetMembers(BindingFlags.Public ||| BindingFlags.Static) |> Seq.filter (fun m -> m.Name = memberAccessExpr.memberName) |> Seq.toList
                    match env.resolveNativeMember methodInfos tid with
                    | [memberInfo, tid] ->
                        match memberInfo with
                        | :? MethodInfo as methodInfo ->
                            Hir.Expr.MemberAccess(Hir.Member.NativeMethod methodInfo, None, tid, memberAccessExpr.span)
                        | :? FieldInfo as fieldInfo ->
                            Hir.Expr.MemberAccess(Hir.Member.NativeField fieldInfo, None, tid, memberAccessExpr.span)
                        | :? PropertyInfo as propertyInfo ->
                            Hir.Expr.MemberAccess(Hir.Member.NativeProperty propertyInfo, None, tid, memberAccessExpr.span)
                        | _ -> Hir.Expr.ExprError(sprintf "Unsupported member type for '%s' at %A" memberAccessExpr.memberName memberAccessExpr.span, tid, memberAccessExpr.span)
                    | [] -> Hir.Expr.ExprError(sprintf "Undefined member '%s' for type '%A' at %A" memberAccessExpr.memberName sysType memberAccessExpr.span, tid, memberAccessExpr.span)
                    | _ -> Hir.Expr.ExprError(sprintf "Ambiguous member '%s' for type '%A' at %A" memberAccessExpr.memberName sysType memberAccessExpr.span, tid, memberAccessExpr.span)
                | _ -> Hir.Expr.ExprError(sprintf "Type '%A' does not support member access at %A" symInfo.typ memberAccessExpr.span, tid, memberAccessExpr.span)
            | None -> Hir.Expr.ExprError(sprintf "Undefined type '%A' at %A" receiver.typ memberAccessExpr.span, tid, memberAccessExpr.span)
        | :? Ast.Expr.If as ifExpr ->
            let rec analyzeIfBranches (branches: Ast.IfBranch list) : Hir.Expr =
                match List.head branches with
                | :? Ast.IfBranch.Then as thenBranch ->
                    let cond = analyzeExpr env thenBranch.cond TypeId.Bool
                    let body = analyzeExpr env thenBranch.body tid
                    Hir.Expr.If(cond, body, analyzeIfBranches (List.tail branches), tid, { left = thenBranch.span.left; right = (List.last branches).span.right })
                | :? Ast.IfBranch.Else as elseBranch ->
                    analyzeExpr env elseBranch.body tid
                | _ -> failwith "Unsupported if branch type"
            analyzeIfBranches ifExpr.branches
        | _ -> failwith "Unsupported expression type"

    and analyzeStmt (env: Env) (stmt: Ast.Stmt) : Hir.Stmt =
        match stmt with
        | :? Ast.Stmt.Let as letStmt ->
            // 先に右辺を解析して型を推論する
            let tid = TypeId.freshMeta ()
            let rhs = analyzeExpr env letStmt.value tid
            // ローカル変数として宣言する
            let sid = env.declareLocal letStmt.name tid
            Hir.Stmt.Let(sid, false, rhs, letStmt.span)
        | :? Ast.Stmt.Var as varStmt ->
            // 先に右辺を解析して型を推論する
            let tid = TypeId.freshMeta ()
            let rhs = analyzeExpr env varStmt.value tid
            // ミュータブルなローカル変数として宣言する
            let sid = env.declareLocal varStmt.name tid
            Hir.Stmt.Let(sid, true, rhs, varStmt.span)
        | :? Ast.Stmt.Assign as assignStmt ->
            // 先に右辺を解析して型を推論する
            let tid = TypeId.freshMeta ()
            let rhs = analyzeExpr env assignStmt.value tid
            // シンボルテーブルで代入先を解決する
            match env.resolveVar assignStmt.name rhs.typ with
            | [sid] ->
                // 代入先が見つかった場合は、Assignステートメントを生成する
                Hir.Stmt.Assign(sid, rhs, assignStmt.span)
            | [] ->
                // 代入先が見つからない場合は、エラーを生成する
                Hir.Stmt.ExprStmt(Hir.Expr.ExprError(sprintf "Undefined variable '%s' at %A" assignStmt.name assignStmt.span, TypeId.Error (sprintf "Undefined variable '%s'" assignStmt.name), assignStmt.span), assignStmt.span)
            | _ -> failwith (sprintf "Ambiguous variable '%s' at %A" assignStmt.name assignStmt.span)
        | :? Ast.Stmt.ExprStmt as exprStmt ->
            let expr = analyzeExpr env exprStmt.expr (TypeId.freshMeta ())
            Hir.Stmt.ExprStmt(expr, exprStmt.span)
        | _ -> failwith "Unsupported statement type"

    let analyzeImport (env: Env) (importDecl: Ast.Decl.Import) : unit =
        let classPath = String.concat "." importDecl.path
        // TODO: 今はSystem.Typeのみをサポートしているが、将来的にはユーザー定義型やモジュールもサポートする必要がある
        env.declareSystemType classPath |> ignore

    let analyzeMethod (env: Env) (fnDecl: Ast.Decl.Fn) : Hir.Method =
        let bodyEnv = env.sub()

        // 返り値の型を名前解決する
        let retType = env.resolveTypeExpr fnDecl.ret

        // 引数の型を名前解決する
        let argTypes = fnDecl.args |> List.map bodyEnv.resolveArgType

        fnDecl.args
        |> List.iteri (fun index arg ->
            match arg with
            | :? Ast.FnArg.Named as namedArg ->
                let argType = argTypes.[index]
                bodyEnv.declareArg namedArg.name argType |> ignore
            | :? Ast.FnArg.Unit -> ()
            | _ -> failwith "Unsupported function argument type")

        let tid = TypeId.Fn(argTypes, retType)

        // 関数のシンボルを定義する
        let sid = env.declareLocal fnDecl.name tid
        // 関数本体を解析する
        let body = analyzeExpr bodyEnv fnDecl.body retType

        Hir.Method(sid, body, tid, fnDecl.span)

    let private collectExprErrors (expr: Hir.Expr) : string list =
        let rec loopExpr (acc: string list) (expr: Hir.Expr) : string list =
            match expr with
            | Hir.Expr.ExprError (message, _, _) -> message :: acc
            | Hir.Expr.Block (stmts, body, _, _) ->
                let accFromStmts = stmts |> List.fold loopStmt acc
                loopExpr accFromStmts body
            | Hir.Expr.If (cond, thenBranch, elseBranch, _, _) ->
                let accWithCond = loopExpr acc cond
                let accWithThen = loopExpr accWithCond thenBranch
                loopExpr accWithThen elseBranch
            | Hir.Expr.Call (_, instance, args, _, _) ->
                let accWithInstance =
                    match instance with
                    | Some inst -> loopExpr acc inst
                    | None -> acc
                args |> List.fold loopExpr accWithInstance
            | Hir.Expr.Lambda (_, _, body, _, _) ->
                loopExpr acc body
            | Hir.Expr.MemberAccess (_, instance, _, _) ->
                match instance with
                | Some inst -> loopExpr acc inst
                | None -> acc
            | _ -> acc

        and loopStmt (acc: string list) (stmt: Hir.Stmt) : string list =
            match stmt with
            | Hir.Stmt.ErrorStmt (message, _) -> message :: acc
            | Hir.Stmt.Let (_, _, value, _)
            | Hir.Stmt.Assign (_, value, _)
            | Hir.Stmt.ExprStmt (value, _) -> loopExpr acc value

        loopExpr [] expr |> List.rev

    let analyzeModule (symbolTable: SymbolTable, typeSubst: TypeSubst, moduleName: string, moduleAst: Ast.Module) : Result<Hir.Module, string list> =
        let moduleScope = Scope(None)
        moduleScope.DeclareType("Unit", TypeId.Unit)
        moduleScope.DeclareType("Bool", TypeId.Bool)
        moduleScope.DeclareType("Int", TypeId.Int)
        moduleScope.DeclareType("Float", TypeId.Float)
        moduleScope.DeclareType("String", TypeId.String)

        symbolTable.BuiltinOperators
        |> List.iter (fun (name, sid) -> moduleScope.DeclareVar(name, sid))

        let env = Env(symbolTable, typeSubst, moduleScope)

        let fields = List<Hir.Field>()
        let methods = List<Hir.Method>()
        let types = List<Hir.Type>()
        for decl in moduleAst.decls do
            match decl with
            | :? Ast.Decl.Import as importDecl ->
                analyzeImport env importDecl
            | :? Ast.Decl.Fn as fnDecl ->
                methods.Add(analyzeMethod env fnDecl)
            | _ -> failwith "Unsupported declaration type in module"

        let hirModule = Hir.Module(moduleName, types |> Seq.toList, fields |> Seq.toList, methods |> Seq.toList, moduleScope)
        let diagnostics =
            hirModule.methods
            |> List.collect (fun m -> collectExprErrors m.body)

        match diagnostics with
        | [] -> Result.Ok hirModule
        | _ -> Result.Error diagnostics
