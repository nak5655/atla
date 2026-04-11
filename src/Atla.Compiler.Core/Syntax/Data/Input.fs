namespace Atla.Compiler.Core.Syntax.Data

open Atla.Compiler.Core.Data

type Input<'T when 'T :> HasSpan> =
    abstract get: Position -> 'T option
    abstract next: Position -> Position
