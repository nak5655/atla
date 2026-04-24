# 04. Closure Conversion Phase Notes

## Phase role
- Closure Conversion is an explicit compiler phase between HIR and Frame Allocation.
- It rewrites lambda-related high-level constructs into closure-normalized representations suitable for low-level layout.

## Invariants
- Input: typed HIR with semantic information complete.
- Output: closure-converted IR (e.g., `ClosedHir`) that contains explicit closure/env artifacts for downstream lowering.
- Deterministic capture ordering and symbol allocation must be preserved.
- Phase boundaries must remain explicit in compile pipeline wiring.

## Retained design decisions from archive
- Closure conversion was moved out of hidden Layout internals and made explicit in the top-level compile pipeline.
- Captured and non-captured lambdas follow distinct lowering strategies with deterministic metadata mapping.
- Capture analysis and rewrite steps were separated to reduce mixed concerns and improve maintainability.
- Closure-specific metadata ownership was separated from base HIR model to keep HIR semantics clean.

## Why retained
This phase is the bridge that removes high-level lambda semantics before frame/memory layout and prevents responsibility leakage into adjacent phases.
