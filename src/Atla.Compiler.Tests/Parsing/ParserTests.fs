namespace Atla.Compiler.Tests.Ast

open System
open Xunit
open Atla.Compiler.Parsing
open Atla.Compiler.Types

module ParserTests =
    [<Fact>]
    let ``hello`` () =
        let program = """
fn greeting: () = do
    var a = 1
    Console.WriteLine "Hello, \"World\"!"
    a
"""
        let input: Input<SourceChar> = StringInput program
        let tokens = Lexer.tokenize input Position.Zero
        match tokens with
            | Success (tokens, _) ->
                let tokenInput = TokenInput(tokens)
                let result = Parser.fileModule() tokenInput tokens.Head.span.left
                Assert.True(result.IsSuccess)
            | Failure (reason, span) ->
                Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let helloEnum () =
        let program = """
role Area =
    def area(self: &Self) -> int

enum Shape = 
    | Rect(w: Int, h: Int)
    | Triangle (b: Int) (h: Int)

impl Shape as Area =
    fn area (self: &Self): int =
        match self
            | Rect w h -> w * h
            | Triangle b h -> b * h / 2
"""
        let input: Input<SourceChar> = StringInput program
        let tokens = Lexer.tokenize input Position.Zero
        match tokens with
            | Success (tokens, _) ->
                let tokenInput = TokenInput(tokens)
                let result = Parser.fileModule() tokenInput tokens.Head.span.left
                Assert.True(result.IsSuccess)
            | Failure (reason, span) ->
                Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
