namespace Atla.Compiler.Tests

open System
open Xunit
open Atla.Compiler.Parsing
open Atla.Compiler.Types

module LexerTests =
    [<Fact>]
    let ``hello`` () =
        let program = """let greeting = " hello !";var  a=-1-0.2 """
        let input: Input<SourceChar> = StringInput program
        let result = Lexer.tokenize input Position.Zero
        Assert.NotNull(Lexer.keywords)

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
