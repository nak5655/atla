namespace Atla.Compiler.Core.Syntax.Data

open Atla.Compiler.Core.Data

type Token = inherit HasSpan

module Token =
    type Id(str: string, span: Span) = 
        member this.str = str
        member this.span = span
        interface Token with
            member this.span = span

    type Int(value: int, span: Span) =
        member this.value = value
        member this.span = span
        interface Token with
            member this.span = span

    type Float(value: float, span: Span) =
        member this.value = value
        member this.span = span
        interface Token with
            member this.span = span

    type Char(value: char, span: Span) =
        member this.value = value
        member this.span = span
        interface Token with
            member this.span = span

    type String(value: string, span: Span) =
        member this.value = value
        member this.span = span
        interface Token with
            member this.span = span

    type Keyword(str: string, span: Span) =
        member this.str = str
        member this.span = span
        interface Token with
            member this.span = span
        override this.ToString (): string =
            sprintf "Keyword('%s', %A)" str span

    type Symbol(str: string, span: Span) =
        member this.str = str
        member this.precedence = match str.Chars 0 with
                                    | '*' | '/' | '%' -> 9
                                    | '+' | '-' -> 8
                                    | ':' -> 7
                                    | '=' | '!' -> 6
                                    | '<' | '>' -> 5
                                    | '&' -> 4
                                    | '^' -> 3
                                    | '|' -> 2
                                    | '.' -> 1
                                    | _ -> 0
        member this.span = span
        interface Token with
            member this.span = span
        override this.ToString (): string =
            sprintf "Symbol('%s', %A)" str span

    type Delim(char: char, span: Span) =
        member this.char = char
        member this.span = span
        interface Token with
            member this.span = span
        override this.ToString (): string =
            sprintf "Delim('%c', %A)" char span

    type Newline(indent: int, span: Span) =
        member this.indent = indent
        member this.span = span
        interface Token with
            member this.span = span

    type Eoi(span: Span) =
        member this.span = span
        interface Token with
            member this.span = span
