# Type System

## Source paths

- `src/Atla.Core/Semantics/Data/Type.fs`
- `src/Atla.Core/Semantics/Infer.fs`
- `src/Atla.Core/Semantics/ExprAnalyze.fs`
- `src/Atla.Core/Semantics/Analyze.fs`

## Current policy

The semantic phase must eliminate unresolved type meta variables before HIR is handed to lowering. Feature-specific type rules are documented in the corresponding language note, for example `union.md` and `unit-void.md`.
