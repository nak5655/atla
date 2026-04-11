namespace Atla.Compiler.Core.Semantics.Data

type SymbolId =
    | SymbolId of int

    member this.id =
        let (SymbolId id) = this
        id
