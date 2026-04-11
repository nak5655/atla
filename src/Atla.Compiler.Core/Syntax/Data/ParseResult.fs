namespace Atla.Compiler.Core.Syntax.Data

open Atla.Compiler.Core.Data

type ParseResult<'A> =
    | Success of 'A * Position
    | Failure of string * Span
