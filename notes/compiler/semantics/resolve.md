# Resolve

## Source paths

- `src/Atla.Core/Semantics/Resolve.fs`
- `src/Atla.Core/Semantics/Data/SymbolInfo.fs`
- `src/Atla.Core/Semantics/Data/Scope.fs`

## Current policy

Resolve owns declaration discovery, symbol creation, scope setup, and module-level name resolution before expression analysis and inference consume semantic data.
