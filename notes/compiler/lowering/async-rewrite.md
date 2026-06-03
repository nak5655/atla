# Async Rewrite

## Source paths

- `src/Atla.Core/Lowering/AsyncRewrite.fs`
- `src/Atla.Core/Semantics/Data/ClosedHir.fs`

## Current policy

Async rewrite transforms `ClosedHir` without introducing new type information.
