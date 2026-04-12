namespace Atla.Core.Syntax.Data

open Atla.Core.Data

type Input<'T when 'T :> HasSpan> =
    abstract get: Position -> 'T option
    abstract next: Position -> Position
