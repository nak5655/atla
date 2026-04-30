namespace Atla.Core.Tests.Syntax

open Xunit
open Atla.Core.Data
open Atla.Core.Syntax
open Atla.Core.Syntax.Data

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

    [<Fact>]
    let ``token spans keep full token width`` () =
        let program = "fn main: Int = 0"
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Success(tokens, _) ->
            let keywordToken =
                tokens
                |> List.pick (fun token ->
                    match token with
                    | :? Token.Keyword as keyword when keyword.str = "fn" -> Some keyword
                    | _ -> None)

            let spanWidth = keywordToken.span.right.Column - keywordToken.span.left.Column
            Assert.Equal(keywordToken.str.Length, spanWidth)
        | Failure(reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``tokenize ignores hash line comments`` () =
        let program = "# file header\nlet answer = 42 # trailing"
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Success(tokens, _) ->
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.Keyword as kw -> kw.str = "let"
                | _ -> false)
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.Int as intToken -> intToken.value = 42
                | _ -> false)
            Assert.DoesNotContain(tokens, fun token ->
                match token with
                | :? Token.Delim as delim -> delim.char = '#'
                | _ -> false)
        | Failure(reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``tokenize keeps hash inside string literals`` () =
        let program = "let s = \"#not-comment\""
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Success(tokens, _) ->
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.String as str -> str.value = "#not-comment"
                | _ -> false)
        | Failure(reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
