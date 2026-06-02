# Guidelines

## Compilation Flow

```
Source Text
  → [Parse] → AST
  → [Semantic Analysis] → HIR
  → [Closure Conversion] → ClosedHir
  → [Async Rewrite] → ClosedHir (transformed)
  → [Frame Allocation / Layout] → MIR
  → [Code Generation] → CIL (.dll)
```

## Phase Invariants

- **Parse**: Source text information must not be carried over past the AST.
- **Semantic Analysis**: Modules are analyzed in topological order. The type meta factory is shared across all modules.
- **Closure Conversion**: Depends only on HIR; must not carry knowledge of MIR or CIL generation.
- **Async Rewrite**: Transforms ClosedHIR without introducing new type information.
- **Frame Allocation (Layout)**: References only ClosedHIR type information.
- **Code Generation**: Takes only MIR as input; must not reference upper-level IRs.

## IR Invariants

- **AST**: Structural representation of source text only. Contains no type or scope information.
- **HIR**: All types fully resolved; no unresolved type meta variables. Scope information is managed in the SymbolTable.
- **ClosedHir**: All HIR invariants hold, plus all free-variable captures are explicitly represented.
- **MIR**: Variable names are indexed; `TypeMeta` is eliminated. Contains only the information needed for CIL emission.
- **CIL (.dll)**: Executable .NET assembly.

## Developer Notes

- Document implementation policies per feature unit in `notes/`.
- Always keep notes up to date as implementation evolves.
- Maintain the hierarchical structure of feature units within `notes/`.
