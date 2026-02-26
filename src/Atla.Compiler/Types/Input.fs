namespace Atla.Compiler.Types

type Input<'T when 'T :> HasSpan> =
    abstract get: Position -> 'T option
    abstract next: Position -> Position
