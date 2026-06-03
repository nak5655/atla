# ClosedHir

## Source paths

- `src/Atla.Core/Semantics/Data/ClosedHir.fs`
- `src/Atla.Core/Lowering/ClosureConversion.fs`
- `src/Atla.Core/Lowering/AsyncRewrite.fs`

## Current policy

`ClosedHir` carries the HIR invariants plus explicit free-variable capture information required by later lowering.
