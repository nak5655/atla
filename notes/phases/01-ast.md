# 01. AST Phase Notes

## AST invariants
- AST represents source syntax only.
- AST must not include resolved symbols or inferred types.
- Source spans are preserved for all nodes.

## Retained design decisions from archive
- Anonymous function syntax (`fn arg1 arg2 -> expr`) was introduced at parser/AST level and later normalized downstream.
- Loop and indexing syntax iterations were settled so that user syntax remains source-facing while semantics handles normalization.
- Syntax evolution (e.g., index expression style migration) is documented as parser-facing behavior, not semantic-layer shortcuts.

## Why retained
These decisions define which transformations are legal at later phases and prevent leakage of typed/runtime concepts into syntax IR.
