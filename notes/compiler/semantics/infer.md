# Infer

## Source paths

- `src/Atla.Core/Semantics/Infer.fs`
- `src/Atla.Core/Semantics/Data/Type.fs`

## Current policy

Inference owns unification and type meta substitution. HIR must not be emitted with unresolved type meta variables.
