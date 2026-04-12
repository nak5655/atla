namespace Atla.Core.Syntax.Data

open Atla.Core.Data

type SourceChar =
    { char: char
    ; span: Span }
    
    interface HasSpan with
        member this.span = this.span
