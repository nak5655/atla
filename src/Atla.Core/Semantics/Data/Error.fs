namespace Atla.Core.Semantics.Data

open Atla.Core.Data

type Error(message: string, span: Span) =
    member this.message = message
    member this.span = span
    member this.toString() = $"{this.message} at {this.span}"
