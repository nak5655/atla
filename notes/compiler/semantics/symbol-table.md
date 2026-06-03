# Symbol Table

## Source paths

- `src/Atla.Core/Semantics/Data/SymbolId.fs`
- `src/Atla.Core/Semantics/Data/SymbolInfo.fs`
- `src/Atla.Core/Semantics/Data/Scope.fs`

## Current policy

Scope information belongs in the symbol table and associated semantic data, not in HIR expression nodes.
