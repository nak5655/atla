# 04. Frame Allocation Phase Notes

## Frame Allocation invariants
- Input must be typed/closure-normalized HIR representation.
- Output must be MIR-compatible frame metadata and lowered instructions.
- No residual high-level lambda constructs may remain at this point.

## Retained design decisions from archive
- Closure conversion was made explicit in compile pipeline wiring before layout/frame allocation.
- Layout error handling was migrated from exceptions to `Result` diagnostics with span-aware failures.
- Mutable frame/label state in MIR-related allocation workflow was eliminated in favor of immutable state passing.
- Remaining lambda detection was kept as a defensive invariant check after closure preprocessing.

## Why retained
This phase is the boundary where semantic IR becomes executable low-level structure; hidden side effects here caused multiple historic regressions.
