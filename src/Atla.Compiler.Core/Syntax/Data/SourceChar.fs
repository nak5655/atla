namespace Atla.Compiler.Core.Syntax.Data

open Atla.Compiler.Core.Data

type SourceChar =
    { char: char
    ; span: Span }
    
    interface HasSpan with
        member this.span = this.span
