namespace Atla.Core.Tests.Syntax

open Xunit
open Atla.Core.Data
open Atla.Core.Syntax
open Atla.Core.Syntax.Data

module LexerTests =
    [<Fact>]
    let ``tokenize parses keywords and literals`` () =
        let program = "val answer = 42"
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            Assert.NotEmpty(tokens)
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.Keyword as kw -> kw.str = "val"
                | _ -> false)
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.Int as intToken -> intToken.value = 42
                | _ -> false)
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``token spans keep full token width`` () =
        let program = "fn main: Int\n    0"
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
        let program = "# file header\nval answer = 42 # trailing"
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Success(tokens, _) ->
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.Keyword as kw -> kw.str = "val"
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
        let program = "val s = \"#not-comment\""
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Success(tokens, _) ->
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.String as str -> str.value = "#not-comment"
                | _ -> false)
        | Failure(reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``tokenize recognizes enum and match keywords`` () =
        let program = "enum Color match value"
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Success(tokens, _) ->
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.Keyword as kw -> kw.str = "enum"
                | _ -> false)
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.Keyword as kw -> kw.str = "match"
                | _ -> false)
        | Failure(reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``tokenize recognizes union object and extendable keywords`` () =
        let program = "extendable union Color object RichBlack"
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Success(tokens, _) ->
            for expected in [ "extendable"; "union"; "object" ] do
                Assert.Contains(tokens, fun token ->
                    match token with
                    | :? Token.Keyword as kw -> kw.str = expected
                    | _ -> false)
        | Failure(reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``tokenizeAll emits standalone and trailing comments with spans`` () =
        let program = "# file header\nval answer = 42 # trailing"
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenizeAll input Position.Zero with
        | Success(tokens, _) ->
            let comments =
                tokens
                |> List.choose (fun token ->
                    match token with
                    | :? Token.Comment as c -> Some c
                    | _ -> None)
            Assert.Equal(2, List.length comments)

            let header = comments.[0]
            Assert.Equal("# file header", header.text)
            Assert.Equal(0, header.span.left.Line)
            Assert.Equal(0, header.span.left.Column)
            Assert.Equal(0, header.span.right.Line)
            Assert.Equal("# file header".Length, header.span.right.Column)

            let trailing = comments.[1]
            Assert.Equal("# trailing", trailing.text)
            Assert.Equal(1, trailing.span.left.Line)

            // Regular tokens are still produced alongside comments.
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.Keyword as kw -> kw.str = "val"
                | _ -> false)
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.Int as i -> i.value = 42
                | _ -> false)
        | Failure(reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``tokenizeAll does not treat hash inside string literals as comment`` () =
        let program = "val s = \"#not-comment\""
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenizeAll input Position.Zero with
        | Success(tokens, _) ->
            Assert.DoesNotContain(tokens, fun token ->
                match token with
                | :? Token.Comment -> true
                | _ -> false)
            Assert.Contains(tokens, fun token ->
                match token with
                | :? Token.String as str -> str.value = "#not-comment"
                | _ -> false)
        | Failure(reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
