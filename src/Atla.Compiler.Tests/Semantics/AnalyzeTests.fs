namespace Atla.Compiler.Tests.Semantics

open Xunit
open Atla.Compiler.Data
open Atla.Compiler.Syntax.Data
open Atla.Compiler.Semantics
open Atla.Compiler.Semantics.Data

module AnalyzeTests =
    [<Fact>]
    let ``analyzeMethod infers argument reference type`` () =
        let span = Span.Empty
        let argType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let retType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let arg = Ast.FnArg.Named("x", argType, span) :> Ast.FnArg
        let body = Ast.Expr.Id("x", span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("id", [arg], retType, body, span) :> Ast.Decl
        let astModule = Ast.Module([fnDecl])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | Result.Ok hirModule ->
            match hirModule.methods.Head.body with
            | Hir.Expr.Id (_, typ, _) -> Assert.Equal(TypeId.Int, Type.resolve subst typ)
            | other -> Assert.True(false, $"expected Hir.Expr.Id but got {other}")
        | Result.Error diagnostics ->
            let message =
                diagnostics
                |> List.map (fun err -> $"{err.message} at {err.span}")
                |> String.concat "; "
            Assert.True(false, $"semantic analysis failed: {message}")

    [<Fact>]
    let ``ast to hir should not keep error nodes`` () =
        let span = Span.Empty
        let importDecl = Ast.Decl.Import([ "System"; "Console" ], span) :> Ast.Decl
        let retType = Ast.TypeExpr.Unit(span) :> Ast.TypeExpr
        let writeLineExpr = Ast.Expr.StaticAccess("Console", "WriteLine", span) :> Ast.Expr
        let helloArg = Ast.Expr.String("Hello, World!", span) :> Ast.Expr
        let callExpr = Ast.Expr.Apply(writeLineExpr, [ helloArg ], span) :> Ast.Expr
        let bodyStmt = Ast.Stmt.ExprStmt(callExpr, span) :> Ast.Stmt
        let body = Ast.Expr.Block([ bodyStmt ], span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, span) :> Ast.Decl
        let astModule = Ast.Module([ importDecl; fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | Result.Ok hirModule ->
            let hasError =
                hirModule.methods
                |> List.exists (fun m -> m.hasError)

            Assert.False(hasError, "HIR に ExprError/ErrorStmt が残っています。")
        | Result.Error diagnostics ->
            let message =
                diagnostics
                |> List.map (fun err -> $"{err.message} at {err.span}")
                |> String.concat "; "
            Assert.True(false, $"semantic analysis failed: {message}")
