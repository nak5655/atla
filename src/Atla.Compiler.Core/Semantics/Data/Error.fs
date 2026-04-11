namespace Atla.Compiler.Core.Semantics.Data

open Atla.Compiler.Core.Data

type Error(message: string, span: Span) =
    member this.message = message
    member this.span = span
    member this.toString() = $"{this.message} at {this.span}"
