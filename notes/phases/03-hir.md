# 03. HIR Phase Notes

## HIR invariants
- HIR is fully typed and immutable.
- HIR contains no unresolved identifiers.
- HIR avoids parser-level sugar.
- HIR should not carry post-closure-conversion artifacts.

## Retained design decisions from archive
- Converted-node cases (`EnvFieldLoad`, `ClosureCreate`) were removed from base HIR and moved to `ClosedHir` to preserve HIR role clarity.
- `closureInvokeMethods` ownership was moved away from generic HIR module shape into closure-converted representation.
- Reusable HIR traversal infrastructure (`map/fold` and context-aware folds) was introduced to reduce missed-case regressions.
- SymbolId allocation was centralized to avoid phase-local ID collision risks.

## Why retained
These choices keep HIR as the typed semantic boundary and prevent leaking lowering artifacts backward.
