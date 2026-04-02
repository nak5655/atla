namespace Atla.Compiler.Semantics

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

        member this.declareLocal (name: string) (typ: TypeId) : SymbolId =
            let symInfo = SymbolInfo(name, typ, SymbolKind.Local())
            let sym = symbolTable.Add(symInfo)
            scope.DeclareVar(name, sym)
            sym

        // シンボルテーブルにローカル変数を追加し、スコープに宣言する
        member this.declareLocalMeta (name: string) : SymbolId =
            let symInfo = SymbolInfo(name, TypeId.Meta (TypeMeta.fresh ()), SymbolKind.Local())
            let sym = symbolTable.Add(symInfo)
            scope.DeclareVar(name, sym)
            sym
            
        member this.declareArg (name: string) (typ: TypeId) : SymbolId =
            let symInfo = SymbolInfo(name, typ, SymbolKind.Arg())
            let sym = symbolTable.Add(symInfo)
            scope.DeclareVar(name, sym)
            sym

        member this.resolveVar (name: string) (expected: TypeId) : SymbolId list =
            scope.ResolveVar(name, expected)

        member this.declareSystemType (classPath: string) : SymbolId =
            let name = Array.last (classPath.Split('.'))
            let tid = TypeId.System(classPath)
            let kind = SymbolKind.SystemType(System.Type.GetType(classPath))
            let symInfo = SymbolInfo(name, tid, kind)
            let sym = symbolTable.Add(symInfo)
            scope.DeclareType(name, sym)
            sym

        member this.resolveSymType (sym: SymbolId) : TypeId =
            match symbolTable.TryGetValue(sym) with
            | Some(symInfo) -> symInfo.typ
            | _ -> TypeId.Error (sprintf "Undefined symbol '%A'" sym)

        member this.unifyTypes (t1: TypeId) (t2: TypeId) : unit =
            Type.unify typSubst t1 t2 |> ignore

        member this.sub(): Env =
            let blockScope = Scope(Some this.scope)
            Env(this.symbolTable, this.typSubst, blockScope)


    let rec analyzeExpr (env: Env) (expr: Ast.Expr) (expected: TypeId) : Hir.Expr =
        match expr with
        | :? Ast.Expr.Unit as unitExpr ->
            env.unifyTypes expected TypeId.Unit
            Hir.Expr.Unit(unitExpr.span)
        | :? Ast.Expr.Int as intExpr ->
            env.unifyTypes expected TypeId.Int
            Hir.Expr.Int(intExpr.value, intExpr.span)
        | :? Ast.Expr.Float as floatExpr ->
            env.unifyTypes expected TypeId.Float
            Hir.Expr.Float(floatExpr.value, floatExpr.span)
        | :? Ast.Expr.String as stringExpr ->
            env.unifyTypes expected TypeId.String
            Hir.Expr.String(stringExpr.value, stringExpr.span)
        | :? Ast.Expr.Id as idExpr ->
            match env.resolveVar idExpr.name expected with
            | [sym] ->
                env.unifyTypes expected (env.resolveSymType sym)
                Hir.Expr.Id(sym, env.resolveSymType sym, idExpr.span)
            | [] -> Hir.Expr.ExprError(sprintf "Undefined variable '%s' at %A" idExpr.name idExpr.span, TypeId.Error (sprintf "Undefined variable '%s'" idExpr.name), idExpr.span)
            | _ -> Hir.Expr.ExprError(sprintf "Ambiguous variable '%s' at %A" idExpr.name idExpr.span, TypeId.Error (sprintf "Ambiguous variable '%s'" idExpr.name), idExpr.span)
        | :? Ast.Expr.Block as blockExpr ->
            let blockEnv = env.sub()
            let stmts = blockExpr.stmts |> List.map (analyzeStmt blockEnv)
            match List.last stmts with
            | Hir.Stmt.ExprStmt (expr, _) ->
                env.unifyTypes expected expr.typ
                Hir.Expr.Block(stmts, expr, expected, blockExpr.span)
            | _ ->
                // ブロックの最後が式でない場合は、ブロック全体の値はUnitとする
                let unitExpr = Hir.Expr.Unit ({left = blockExpr.span.right; right = blockExpr.span.right})
                Hir.Expr.Block(stmts, unitExpr, expected, blockExpr.span)
        | :? Ast.Expr.Apply as applyExpr ->
            // 関数本体の名前解析と型推論を行う
            let func = analyzeExpr env applyExpr.func (TypeId.Arrow (TypeId.freshMeta (), expected))
            // 引数の名前解析と型推論を行いながら、Applyに畳み込む
            List.tail (applyExpr.args) |> List.fold (fun acc arg ->
                let argExpr = analyzeExpr env arg (TypeId.freshMeta())
                // TODO: Applyの型は、関数の型から引数の型を引いていく形で推論する必要がある
                Hir.Expr.Apply(acc, argExpr, TypeId.freshMeta(), applyExpr.span)) func
        | :? Ast.Expr.MemberAccess as memberAccessExpr ->
            // TODO: メンバーアクセスの解析は、まずレシーバーの型を推論し、その型に対してメンバー名で名前解決を行う必要がある
            let receiver = analyzeExpr env memberAccessExpr.receiver (TypeId.freshMeta())
            Hir.Expr.MemberAccess(receiver, memberAccessExpr.memberName, TypeId.freshMeta(), memberAccessExpr.span)
        | :? Ast.Expr.If as ifExpr ->
            let rec analyzeIfBranches (branches: Ast.IfBranch list) : Hir.Expr =
                match List.head branches with
                | :? Ast.IfBranch.Then as thenBranch ->
                    let cond = analyzeExpr env thenBranch.cond TypeId.Bool
                    let body = analyzeExpr env thenBranch.body expected
                    Hir.Expr.If(cond, body, analyzeIfBranches (List.tail branches), expected, { left = thenBranch.span.left; right = (List.last branches).span.right })
                | :? Ast.IfBranch.Else as elseBranch ->
                    analyzeExpr env elseBranch.body expected
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
            let sym = env.declareLocal letStmt.name tid
            Hir.Stmt.Let(sym, false, rhs, letStmt.span)
        | :? Ast.Stmt.Var as varStmt ->
            // 先に右辺を解析して型を推論する
            let tid = TypeId.freshMeta ()
            let rhs = analyzeExpr env varStmt.value tid
            // ミュータブルなローカル変数として宣言する
            let sym = env.declareLocal varStmt.name tid
            Hir.Stmt.Let(sym, true, rhs, varStmt.span)
        | :? Ast.Stmt.Assign as assignStmt ->
            // 先に右辺を解析して型を推論する
            let tid = TypeId.freshMeta ()
            let rhs = analyzeExpr env assignStmt.value tid
            // シンボルテーブルで代入先を解決する
            match env.resolveVar assignStmt.name rhs.typ with
            | [sym] ->
                // 代入先が見つかった場合は、Assignステートメントを生成する
                Hir.Stmt.Assign(sym, rhs, assignStmt.span)
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

    let analyzeFn (env: Env) (fnDecl: Ast.Decl.Fn) : Hir.Field =
        let bodyEnv = env.sub()

        // 返り値の型を名前解決する
        let retType = env.resolveTypeExpr fnDecl.ret

        // 引数の型を名前解決し、Arrow型に折り畳む
        let tid = List.rev fnDecl.args |> List.fold (fun acc (arg: Ast.FnArg) ->
            match arg with
            | :? Ast.FnArg.Named as namedArg ->
                let argType = bodyEnv.resolveArgType arg
                bodyEnv.declareArg namedArg.name argType |> ignore
                TypeId.Arrow(argType, acc)
            | _ -> TypeId.Arrow(TypeId.Unit, acc)) retType

        // 関数のシンボルを定義する
        let sym = env.declareLocal fnDecl.name tid

        // 関数本体を解析する
        let body = analyzeExpr bodyEnv fnDecl.body (TypeId.freshMeta ())

        Hir.Field(sym, tid, body, fnDecl.span)

    let analyzeModule (symbolTable: SymbolTable, typeSubst: TypeSubst, moduleName: string, moduleAst: Ast.Module) : Hir.Module =
        let moduleScope = Scope(Some (Scope.GlobalScope()))
        let env = Env(symbolTable, typeSubst, moduleScope)

        let fields = List<Hir.Field>()
        let types = List<Hir.Type>()
        for decl in moduleAst.decls do
            match decl with
            | :? Ast.Decl.Import as importDecl ->
                analyzeImport env importDecl
            | :? Ast.Decl.Fn as fnDecl ->
                fields.Add(analyzeFn env fnDecl)
            | _ -> failwith "Unsupported declaration type in module"

        Hir.Module(moduleName, types |> Seq.toList, fields |> Seq.toList, moduleScope)