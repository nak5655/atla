# Notes Index

This directory consolidates historical planning and design documentation.

## Contents
- `plans-archive.md`: Archived historical plan entries.
- `phases/README.md`: Compiler notes reorganized by phase (`AST -> Semantic -> HIR -> Closure Conversion -> Frame Allocation -> MIR -> CIL`).
- `build-system-phase1.md`: Build system phase design notes.
- `cli-interface.md`: CLI behavior and interface notes.
- `language-server.md`: Language Server design notes.
- `semantic-phase-design.md`: Semantic phase design details.
- `unit-void-design.md`: Unit/Void semantics design notes.
- `multi-impl-spec.md`: Proposal for allowing multiple `impl` blocks on a single `data` type.
- `union-spec.md`: `union` (tagged sum type) specification — syntax, pattern matching, subtyping, and lowering to an abstract-class/variant hierarchy (replaces the removed `enum`).
- `flow-control-spec.md`: Flow control and loop control specification (`return`, `break`, `continue`, `for`, `while`, `let-else`/`var-else`).

## Usage
- Keep durable design rationale and historical records in this `notes/` directory.
- Add new compiler design decisions to `notes/phases/` in the corresponding phase file.
