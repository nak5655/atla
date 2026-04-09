namespace Atla.Compiler.Tests.Semantics

open Xunit
open Atla.Compiler.Data
open Atla.Compiler.Syntax
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
            let message = String.concat "; " diagnostics
            Assert.True(false, $"semantic analysis failed: {message}")

    [<Fact>]
    let ``ast to hir should not keep error nodes`` () =
        let program = """
import System.Console

fn main: () = do
    Console.WriteLine "Hello, World!"
"""

        let input: Input<SourceChar> = StringInput program

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Success (astModule, _) ->
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | Result.Ok hirModule ->
                    let hasError =
                        hirModule.methods
                        |> List.exists (fun m -> m.hasError)

                    Assert.False(hasError, "HIR に ExprError/ErrorStmt が残っています。")
                | Result.Error diagnostics ->
                    let message = String.concat "; " diagnostics
                    Assert.True(false, $"semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
