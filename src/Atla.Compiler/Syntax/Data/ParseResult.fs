namespace Atla.Compiler.Syntax.Data

open Atla.Compiler.Data

type ParseResult<'A> =
    | Success of 'A * Position
    | Failure of string * Span
