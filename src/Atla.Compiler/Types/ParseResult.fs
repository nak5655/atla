namespace Atla.Compiler.Types

type ParseResult<'A> =
    | Success of 'A * Position
    | Failure of string * Span
