# Compiler Notes

`Atla.Core` implementation notes.

## Contents

- [`pipeline.md`](pipeline.md) — canonical compile flow and cross-phase invariants.
- [`syntax/`](syntax/README.md) — lexer, parser, and AST notes.
- [`semantics/`](semantics/README.md) — resolve/analyze/infer/HIR notes.
- [`lowering/`](lowering/README.md) — closure conversion, async rewrite, layout, and MIR notes.
- [`codegen/`](codegen/README.md) — CIL emission notes.
- [`dependencies/`](dependencies/README.md) — compiler-side dependency loading and `.atlalib` format notes.

## Source paths

- `src/Atla.Core/Compile.fs`
- `src/Atla.Core/AtlaLib.fs`
- `src/Atla.Core/DependencyLoader.fs`
- `src/Atla.Core/Syntax/`
- `src/Atla.Core/Semantics/`
- `src/Atla.Core/Lowering/`
