# 02. Semantic Analysis Phase Notes

## Semantic invariants
- All symbols are resolved before HIR generation.
- Type checking is complete before lowering to HIR.
- No unresolved identifier leaves this phase.
- Diagnostics are explicit, structured, and deterministic.

## Retained design decisions from archive
- Resolve/Infer responsibilities were separated and refined to reduce cross-concern coupling.
- Diagnostic model was expanded so warnings/info can be returned alongside successful analysis.
- `Expression is not callable` diagnostics were improved with root-cause context.
- Unit/Void handling was consolidated in semantics to preserve downstream lowering consistency.
- First-class function typing/callability checks were hardened for GUI and library-heavy workloads.

## Why retained
These rules are durable contracts for all subsequent phases and are repeatedly referenced in regression work.
