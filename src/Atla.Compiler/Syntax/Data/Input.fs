namespace Atla.Compiler.Syntax.Data

open Atla.Compiler.Data

type Input<'T when 'T :> HasSpan> =
    abstract get: Position -> 'T option
    abstract next: Position -> Position
