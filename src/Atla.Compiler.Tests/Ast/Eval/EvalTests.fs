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

fn main () = do
    Console.WriteLine "Hello, World!"
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
                let globalScope = Scope.GlobalScope ()
                Eval.evalModule globalScope hir
                globalScope.GetVar("main")
                    |> Option.map (fun variable ->
                        match variable.value with
                        | Value.Function mainFunc -> Assert.Equal(Value.Unit, mainFunc [Value.Unit])
                        | _ -> Assert.True(false, "Expected 'main' to be a function"))
                    |> Option.defaultWith (fun () -> Assert.True(false, "Variable 'main' not found in global scope"))
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
