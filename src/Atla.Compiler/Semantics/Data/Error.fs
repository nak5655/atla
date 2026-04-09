namespace Atla.Compiler.Semantics.Data

open Atla.Compiler.Data

type Error(message: string, span: Span) =
    member this.message = message
    member this.span = span
    member this.toString() = $"{this.message} at {this.span}"
