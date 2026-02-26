namespace Atla.Compiler.Parsing

open Atla.Compiler.Types

type SourceChar =
    { char: char
    ; span: Span }
    
    interface HasSpan with
        member this.span = this.span
