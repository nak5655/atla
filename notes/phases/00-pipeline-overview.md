# 00. Pipeline Overview

## Canonical phase order
`AST -> Semantic Analysis -> HIR -> Frame Allocation -> MIR -> CIL`

## Durable cross-phase decisions (from archive)
- Compiler boundaries must be explicit modules; hidden internal phase jumps are disallowed.
- Determinism is required for diagnostics and IR output ordering.
- Error handling must stay `Result`-based with structured diagnostics instead of `failwith` control flow.
- Closure conversion is treated as an explicit pre-processing step before frame allocation, not an implicit side effect inside layout.

## Traceability
Key historical batches in `notes/plans-archive.md`:
- 2026-04-22: phase-separation and purity fixes.
- 2026-04-21: closure lowering policy and env-class migration.
- 2026-04-12 and 2026-04-10: semantic/diagnostic restructuring and module responsibility cleanup.
