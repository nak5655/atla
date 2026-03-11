namespace Atla.Compiler.Hir

open Atla.Compiler.Types

type Variable(value: Value, isMutable: bool) =
    let mutable pTyp = TypeCray.Unknown
    member this.value = value
    member this.typ
        with get() = pTyp
        and set(v) = pTyp <- v
    member this.isMutable = isMutable