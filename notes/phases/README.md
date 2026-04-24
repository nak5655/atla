# Compiler Phase Notes Index

This directory distills long-term design decisions from `notes/plans-archive.md` into compiler-phase units.

## Files
- `00-pipeline-overview.md`
- `01-ast.md`
- `02-semantic-analysis.md`
- `03-hir.md`
- `04-closure-conversion.md`
- `05-frame-allocation.md`
- `06-mir.md`
- `07-cil.md`

## Canonical order
`AST -> Semantic Analysis -> HIR -> Closure Conversion -> Frame Allocation -> MIR -> CIL`

## Source of truth
- Historical rationale and implementation chronology remain in `../plans-archive.md`.
- These phase notes keep only durable design decisions and invariants.
