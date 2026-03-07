namespace Atla.Compiler.Hir

type Variable(value: Value, isMutable: bool) =
    member this.value = value
    member this.isMutable = isMutable