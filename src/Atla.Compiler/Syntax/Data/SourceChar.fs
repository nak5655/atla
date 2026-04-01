namespace Atla.Compiler.Syntax.Data

open Atla.Compiler.Data

type SourceChar =
    { char: char
    ; span: Span }
    
    interface HasSpan with
        member this.span = this.span
