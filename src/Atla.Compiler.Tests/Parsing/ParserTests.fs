namespace Atla.Compiler.Tests.Ast

open System
open Xunit
open Atla.Compiler.Parsing
open Atla.Compiler.Types

module ParserTests =
    [<Fact>]
    let ``hello`` () =
        let program = """
let greeting = do
    var a = 1
    a
let b = c"""
        let input: Input<SourceChar> = StringInput program
        let tokens = Lexer.tokenize input Position.Zero
        match tokens with
            | Success (tokens, _) ->
                let tokenInput = TokenInput(tokens)
                let result = Parser.letStmt() tokenInput tokens.Head.span.left
                Assert.True(result.IsSuccess)
            | Failure (reason, span) ->
                Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let helloData () =
        let program = """
type Shape =
    data Rect: Shape =
        var w: int
        var h: int

    data Triangle =
        var b: int
        var h: int

role Area =
    fn area (self: &Self): int

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
                let result = Parser.letStmt() tokenInput tokens.Head.span.left
                Assert.True(result.IsSuccess)
            | Failure (reason, span) ->
                Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
