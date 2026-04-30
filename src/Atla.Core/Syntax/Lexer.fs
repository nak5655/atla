namespace Atla.Core.Syntax

open Atla.Core.Data
open Atla.Core.Syntax.Data
open Atla.Core.Syntax.Combinators

type StringInput(s: string) =
    let normalized = s.Replace("\r\n", "\n").Replace('\r', '\n')
    let lines = normalized.Split('\n')
    interface Input<SourceChar> with
        member _.get (arg: Position): SourceChar option =
            if arg.Line < lines.Length then
                let line = lines.[arg.Line]
                if arg.Column < line.Length then
                    let c = line.[arg.Column]
                    Some { char = c; span = { left = arg; right = arg.Advance(c); }}
                elif arg.Column = line.Length && arg.Line < lines.Length - 1 then
                    Some { char = '\n'; span = { left = arg; right = arg.Advance('\n'); }}
                else None
            else None

        member _.next (arg: Position): Position =
            let line = lines.[arg.Line]
            if arg.Column < line.Length then
                { arg with Column = arg.Column + 1 }
            else
                { Line = arg.Line + 1; Column = 0 }

module Lexer =
    let asToken<'T when 'T :> Token> (p: PackratParser<SourceChar, 'T>) : PackratParser<SourceChar, Token> =
        p |>> fun t -> t :> Token

    let keywords = [
        // declarations
        "let"; "var"; "fn"; "mod"; "def"; "use"; "import"; "data"; "this"; "role"; "impl";
        // control flows
        "for"; "by"; "in"; "if"; "else"; "match"; "do"; "while";
        // block
        "return"; "continue"; "break";
        // boolean
        "True"; "False";
        // arrows
        "->"; "=>";
    ]
    let delims = ['''; '"'; '`'; ','; ';'; ':'; '('; ')'; '['; ']'; '{'; '}']
    let opSigns = ['+'; '-'; '*'; '/'; '%'; '<'; '>'; '='; '!'; '^'; '&'; '|'; '?'; '.']
    
    let ws = AcceptIf (fun c -> System.Char.IsWhiteSpace(c.char))
    /// Parse a single-line comment that starts with '#' and ends at newline or EOF.
    let lineComment: PackratParser<SourceChar, unit> =
        AcceptIf (fun c -> c.char = '#') <&> Many (AcceptIf (fun c -> c.char <> '\n')) |>> fun _ -> ()

    /// Parse whitespace/comment trivia and discard them before/after tokens.
    let trivia: PackratParser<SourceChar, unit> =
        (ws |>> fun _ -> ()) <|> lineComment
    let alpha = AcceptIf (fun c -> System.Char.IsLetter(c.char))
    let alpha_ = alpha <|> (AcceptIf (fun c -> c.char = '_'))
    let digit = AcceptIf (fun c -> System.Char.IsDigit(c.char))
    let nonZeroDigit = AcceptIf (fun c -> System.Char.IsDigit(c.char) && c.char <> '0')
    let intZeroRaw = AcceptIf (fun c -> c.char = '0')
    let intNotZeroRaw = nonZeroDigit <&> Many digit |>> fun (first, rest) -> SourceString.join (first :: rest)
    let intRaw = (intZeroRaw |>> fun c -> { string = "0"; span = c.span }) <|> intNotZeroRaw
    let floatRaw = intRaw <& AcceptIf (fun c -> c.char = '.') <&> intRaw |>> fun (intPart, fracPart) -> { string = intPart.string + "." + fracPart.string; span = { left = intPart.span.left; right = fracPart.span.right } }
    let alphaNum: PackratParser<SourceChar, SourceChar> = alpha <|> digit
    let alphaNum_ = alpha_ <|> digit
    let keyword: PackratParser<SourceChar, Token.Keyword> =
        let isWordKeyword (kw: string) =
            kw |> Seq.last |> System.Char.IsLetterOrDigit || kw.EndsWith("_")
        let isIdentContinuation (c: SourceChar) =
            System.Char.IsLetterOrDigit(c.char) || c.char = '_'

        keywords
        |> List.map (fun kw ->
            let rawKeyword =
                Phrase (kw |> Seq.toList) (fun (c, k) -> c.char = k)
                |>> fun chars ->
                    let s = SourceString.join(chars)
                    Token.Keyword(s.string, s.span)

            if isWordKeyword kw then
                Delay (fun () -> fun input pos ->
                    match rawKeyword input pos with
                    | Success (tok, nextPos) ->
                        match input.get nextPos with
                        | Some nextChar when isIdentContinuation nextChar ->
                            Failure ("Keyword boundary mismatch", tok.span)
                        | _ -> Success (tok, nextPos)
                    | Failure (reason, span) -> Failure (reason, span))
            else
                rawKeyword)
        |> List.fold (<|>) (Fail "No keywords")
    let delim: PackratParser<SourceChar, Token.Delim> =
        delims
            |> List.map (fun d -> AcceptIf (fun (c: SourceChar) -> c.char = d) |>> fun c -> Token.Delim (c.char, c.span))
            |> List.fold (<|>) (Fail "No delimiters")
    let symbol: PackratParser<SourceChar, Token.Symbol> =
        Many1 (AcceptIf (fun c -> opSigns |> List.contains c.char)) |>> fun chars -> let s = SourceString.join(chars) in Token.Symbol(s.string, s.span)
    let id = alpha_ <&> Many alphaNum_ |>> fun (first, rest) -> let s = SourceString.join(first :: rest) in Token.Id(s.string, s.span)
    let int = intRaw |>> fun s -> Token.Int(System.Int32.Parse(s.string), s.span)
    let float = floatRaw |>> fun s -> Token.Float(System.Double.Parse(s.string), s.span)
    let stringEscape: PackratParser<SourceChar, Token.String> =
        AcceptIf (fun c -> c.char = '\\') <&> AcceptIf (fun c -> c.char = '\\') |>> fun (a, b) -> Token.String("\\", { left = a.span.left; right = b.span.right })
        <|> (AcceptIf (fun c -> c.char = '\\') <&> AcceptIf (fun c -> c.char = '"') |>> fun (a, b) -> Token.String("\"", { left = a.span.left; right = b.span.right }))
        <|> (AcceptIf (fun c -> c.char = '\\') <&> AcceptIf (fun c -> c.char = 'n') |>> fun (a, b) -> Token.String("\n", { left = a.span.left; right = b.span.right }))
        <|> (AcceptIf (fun c -> c.char = '\\') <&> AcceptIf (fun c -> c.char = 'r') |>> fun (a, b) -> Token.String("\r", { left = a.span.left; right = b.span.right }))
        <|> (AcceptIf (fun c -> c.char = '\\') <&> AcceptIf (fun c -> c.char = 't') |>> fun (a, b) -> Token.String("\t", { left = a.span.left; right = b.span.right }))
    let string: PackratParser<SourceChar, Token.String> =
        AcceptIf (fun c -> c.char = '"') <&> Many (stringEscape <|> (AcceptIf (fun c -> c.char <> '"') |>> fun c -> Token.String(c.char.ToString(), c.span))) <&> AcceptIf (fun c -> c.char = '"') |>> fun ((openQuote, content), closeQuote) ->
            let strContent = content |> List.map (fun t -> t.value) |> String.concat ""
            Token.String(strContent, { left = openQuote.span.left; right = closeQuote.span.right })

    let tokenize : PackratParser<SourceChar, Token list> = 
        Many (trivia) &> SepBy ((asToken keyword) <|> (asToken string) <|> (asToken delim) <|> (asToken float) <|> (asToken int) <|> (asToken id) <|> (asToken symbol)) (Many trivia) <& Many trivia <& Eoi |>> fun tokens -> tokens
