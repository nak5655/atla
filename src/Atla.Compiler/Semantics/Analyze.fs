namespace Atla.Compiler.Semantics

open System.Collections.Generic
open Atla.Compiler.Syntax.Data
open Atla.Compiler.Semantics.Data

module Analyze =
    let rec analyzeExpr (symbolTable: SymbolTable) (scope: Scope) (expr: Ast.Expr) (expected: TypeId) : Hir.Expr =
        match expr with
        | :? Ast.Expr.Unit as unitExpr -> Hir.Expr.Unit(unitExpr.span)
        | :? Ast.Expr.Int as intExpr -> Hir.Expr.Int(intExpr.value, intExpr.span)
        | :? Ast.Expr.Float as floatExpr -> Hir.Expr.Float(floatExpr.value, floatExpr.span)
        | :? Ast.Expr.String as stringExpr -> Hir.Expr.String(stringExpr.value, stringExpr.span)
        | :? Ast.Expr.Id as idExpr ->
            match scope.ResolveVar(idExpr.name, expected) with
            | [sym] -> Hir.Expr.Id(sym, symbolTable.Get(sym).typ, idExpr.span)
            | [] -> Hir.Expr.ExprError(sprintf "Undefined variable '%s' at %A" idExpr.name idExpr.span, TypeId.Error (sprintf "Undefined variable '%s'" idExpr.name), idExpr.span)
            | _ -> Hir.Expr.ExprError(sprintf "Ambiguous variable '%s' at %A" idExpr.name idExpr.span, TypeId.Error (sprintf "Ambiguous variable '%s'" idExpr.name), idExpr.span)
        | :? Ast.Expr.Block as blockExpr ->
            let blockScope = Scope(Some scope)
            let stmts = blockExpr.stmts |> List.map (analyzeStmt symbolTable blockScope)
            match List.last stmts with
            | Hir.Stmt.ExprStmt (expr, span) ->
                Hir.Expr.Block(stmts, expr, blockScope, expected, blockExpr.span)
            | _ ->
                // ブロックの最後が式でない場合は、ブロック全体の値はUnitとする
                let expr = Hir.Expr.Unit ({left = blockExpr.span.right; right = blockExpr.span.right})
                Hir.Expr.Block(stmts, expr, blockScope, expected, blockExpr.span)
        | :? Ast.Expr.Apply as applyExpr ->
            // 関数本体の名前解析と型推論を行う
            let func = analyzeExpr symbolTable scope applyExpr.func (TypeId.Arrow (TypeId.freshMeta (), expected))
            // 引数の名前解析と型推論を行いながら、Applyに畳み込む
            let acc = analyzeExpr symbolTable scope (List.head (applyExpr.args)) (TypeId.freshMeta())
            List.tail (applyExpr.args) |> List.fold (fun acc arg ->
                let argExpr = analyzeExpr symbolTable scope arg (TypeId.freshMeta())
                Hir.Expr.Apply(acc, argExpr, expected, applyExpr.span)) acc
        | :? Ast.Expr.MemberAccess as memberAccessExpr ->
            let receiver = analyzeExpr symbolTable scope memberAccessExpr.receiver (TypeId.freshMeta())
            let memberName = memberAccessExpr.memberName
            Hir.Expr.MemberAccess(receiver, memberName, TypeId.freshMeta(), memberAccessExpr.span)
        | :? Ast.Expr.If as ifExpr ->
            let rec analyzeIfBranches (branches: (Ast.IfBranch) list) : Hir.Expr =
                match List.head branches with
                | :? Ast.IfBranch.Then as thenBranch ->
                    let cond = analyzeExpr symbolTable scope thenBranch.cond TypeId.Bool
                    let body = analyzeExpr symbolTable scope thenBranch.body expected
                    Hir.Expr.If(cond, body, analyzeIfBranches (List.tail branches), expected, { left = thenBranch.span.left; right = (List.last branches).span.right })
                | :? Ast.IfBranch.Else as elseBranch ->
                    analyzeExpr symbolTable scope elseBranch.body expected
                | _ -> failwith "Unsupported if branch type"
            analyzeIfBranches ifExpr.branches
        | _ -> failwith "Unsupported expression type"

    and analyzeStmt (symbolTable: SymbolTable) (scope: Scope) (stmt: Ast.Stmt) : Hir.Stmt =
        match stmt with
        | :? Ast.Stmt.Let as letStmt ->
            // 先に右辺を解析して型を推論する
            let rhs = analyzeExpr symbolTable scope letStmt.value (TypeId.freshMeta ())
            // シンボルテーブルに変数を追加する
            let symInfo = SymbolInfo(letStmt.name, TypeId.Meta (TypeMeta.fresh ()), SymbolKind.Local())
            let sym = symbolTable.Add(symInfo)
            scope.DeclareVar(letStmt.name, sym)
            Hir.Stmt.Let(sym, false, rhs, letStmt.span)
        | :? Ast.Stmt.Var as varStmt ->
            // 先に右辺を解析して型を推論する
            let rhs = analyzeExpr symbolTable scope varStmt.value (TypeId.freshMeta ())
            // シンボルテーブルに変数を追加する
            let symInfo = SymbolInfo(varStmt.name, TypeId.Meta (TypeMeta.fresh ()), SymbolKind.Local())
            let sym = symbolTable.Add(symInfo)
            scope.DeclareVar(varStmt.name, sym)
            Hir.Stmt.Let(sym, true, rhs, varStmt.span)
        | :? Ast.Stmt.Assign as assignStmt ->
            // 先に右辺を解析して型を推論する
            let rhs = analyzeExpr symbolTable scope assignStmt.value (TypeId.freshMeta ())
            // シンボルテーブルで代入先を解決する
            match scope.ResolveVar(assignStmt.name, rhs.typ) with
            | [sym] ->
                // 代入先が見つかった場合は、Assignステートメントを生成する
                Hir.Stmt.Assign(sym, rhs, assignStmt.span)
            | [] ->
                // 代入先が見つからない場合は、エラーを生成する
                Hir.Stmt.ExprStmt(Hir.Expr.ExprError(sprintf "Undefined variable '%s' at %A" assignStmt.name assignStmt.span, TypeId.Error (sprintf "Undefined variable '%s'" assignStmt.name), assignStmt.span), assignStmt.span)
            | _ -> failwith "Ambiguous variable '%s' at %A" assignStmt.name assignStmt.span
        | :? Ast.Stmt.ExprStmt as exprStmt ->
            let expr = analyzeExpr symbolTable scope exprStmt.expr (TypeId.freshMeta ())
            Hir.Stmt.ExprStmt(expr, exprStmt.span)
        | _ -> failwith "Unsupported statement type"

    let rec resolveTypeExpr (scope: Scope) (typeExpr: Ast.TypeExpr) : TypeId =
        match typeExpr with
        | :? Ast.TypeExpr.Unit -> TypeId.Unit
        | :? Ast.TypeExpr.Id as idTypeExpr -> 
            match scope.ResolveType(idTypeExpr.name) with
            | Some t -> t
            | _ -> TypeId.Error (sprintf "Undefined type '%s' at %A" idTypeExpr.name idTypeExpr.span)
        | _ -> failwith "Unsupported type expression type"
        
    let resolveArgType (scope: Scope) (arg: Ast.FnArg) : TypeId =
        match arg with
        | :? Ast.FnArg.Named as namedArg -> resolveTypeExpr scope namedArg.typeExpr
        | :? Ast.FnArg.Unit -> TypeId.Unit
        | _ -> failwith "Unsupported function argument type"

    let analyzeImport (symbolTable: SymbolTable) (scope: Scope) (importDecl: Ast.Decl.Import) : unit =
        let name = List.last importDecl.path
        let classPass = String.concat "." importDecl.path
        // TODO: 今はSystem.Typeのみをサポートしているが、将来的にはユーザー定義型やモジュールもサポートする必要がある
        let tid = TypeId.System(classPass)
        let kind = SymbolKind.SystemType(System.Type.GetType(classPass))
        let symInfo = SymbolInfo(name, tid, kind)

        let name = List.last importDecl.path
        let sym = symbolTable.Add(symInfo)
        scope.DeclareType(name, sym)
        
    let analyzeFn (symbolTable: SymbolTable) (scope: Scope) (fnDecl: Ast.Decl.Fn) : Hir.Field =
        let bodyScope = Scope(Some scope)

        // 引数と返り値の型を名前解決し、Arrow型に折り畳む
        let ret = resolveArgType scope (List.last fnDecl.args)
        let tid = List.tail (List.rev fnDecl.args) |> List.fold (fun acc (arg: Ast.FnArg) ->
            match arg with
            | :? Ast.FnArg.Named as namedArg ->
                let argType = resolveArgType scope arg
                bodyScope.DeclareVar(namedArg.name, symbolTable.Add(SymbolInfo(namedArg.name, argType, SymbolKind.Arg())))
                TypeId.Arrow(argType, acc)
            | _ -> TypeId.Arrow(TypeId.Unit, acc)) ret

        // 関数のシンボルを定義する
        let name = fnDecl.name
        let kind = SymbolKind.Local ()
        let symInfo = SymbolInfo(name, tid, kind)
        let sym = symbolTable.Add(symInfo)

        // 関数のシンボルをスコープに宣言する
        scope.DeclareVar(name, sym)

        // 関数本体を解析する
        let body = analyzeExpr symbolTable bodyScope fnDecl.body (TypeId.freshMeta ())

        Hir.Field(sym, symInfo.typ, body, fnDecl.span)

    let rec analyzeModule (symbolTable: SymbolTable, typeSubst: TypeSubst, moduleName: string, moduleAst: Ast.Module) : Hir.Module =
        let moduleScope = Scope(Some (Scope.GlobalScope()))
        let fields = List<Hir.Field>()
        let types = List<Hir.Type>()
        for decl in moduleAst.decls do
            match decl with
            | :? Ast.Decl.Import as importDecl -> 
                analyzeImport symbolTable moduleScope importDecl
            | :? Ast.Decl.Fn as fnDecl ->
                let field = analyzeFn symbolTable moduleScope fnDecl
                fields.Add(field)
            | _ -> failwith "Unsupported declaration type in module"

        Hir.Module(moduleName, types |> Seq.toList, fields |> Seq.toList, moduleScope)