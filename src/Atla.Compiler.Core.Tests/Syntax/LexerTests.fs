namespace Atla.Compiler.Core.Tests.Syntax

open Xunit
open Atla.Compiler.Core.Data
open Atla.Compiler.Core.Syntax
open Atla.Compiler.Core.Syntax.Data

module LexerTests =
    [<Fact>]
    let ``tokenize parses keywords and literals`` () =
        let program = "let answer = 42"
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            Assert.NotEmpty(tokens)
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.Keyword as kw -> kw.str = "let"
                | _ -> false)
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.Int as intToken -> intToken.value = 42
                | _ -> false)
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
