# Pipeline Overview

## Source paths

- `GUIDELINES.md`
- `src/Atla.Core/Compile.fs`


## Canonical phase order
`Source Text -> Parse -> AST -> Semantic Analysis -> HIR -> Closure Conversion -> ClosedHir -> Async Rewrite -> ClosedHir -> Frame Allocation / Layout -> MIR -> Code Generation -> CIL`

## Durable cross-phase decisions
- Compiler boundaries must be explicit modules; hidden internal phase jumps are disallowed.
- Determinism is required for diagnostics and IR output ordering.
- Error handling must stay `Result`-based with structured diagnostics instead of `failwith` control flow.
- Closure conversion is an explicit pre-processing phase before frame allocation, not an implicit side effect inside layout.
