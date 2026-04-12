namespace Atla.Core.Syntax.Data

open Atla.Core.Data

type ParseResult<'A> =
    | Success of 'A * Position
    | Failure of string * Span
