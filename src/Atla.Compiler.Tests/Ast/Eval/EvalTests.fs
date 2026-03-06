namespace Atla.Compiler.Tests.Ast.Eval

open System
open Xunit
open Atla.Compiler.Ast
open Atla.Compiler.Types
open Atla.Compiler.Parsing
open Atla.Compiler.Hir
open Atla.Compiler.Hir.Eval
open Atla.Compiler.Lowering

module EvalTests =
    [<Fact>]
    let ``helloEval`` () =
        let program = """
import System.Console

let greeting = do
    var a = 1
    a = a + 2 * 5
"""
        let input: Input<SourceChar> = StringInput program
        let tokens = Lexer.tokenize input Position.Zero
        match tokens with
            | Success (tokens, _) ->
                let tokenInput = TokenInput(tokens)
                let result = Parser.fileModule() tokenInput tokens.Head.span.left
                match result with
                    | Success (moduleAst, _) ->
                        let hir = Desugar.desugarModule moduleAst
                        let globalScope = Scope(None)
                        Eval.evalModule globalScope hir
                        //Assert.Equal(Some(Value.Int 3), globalScope.GetVar("a"))
                    | Failure (reason, span) ->
                        Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
            | Failure (reason, span) ->
                Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
