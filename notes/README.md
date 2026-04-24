# Notes Index

This directory consolidates historical planning and design documentation that was previously split across `PLANS.md` and `doc/`.

## Contents
- `plans-archive.md`: Archived historical plan entries from the previous root `PLANS.md`.
- `phases/README.md`: Compiler notes reorganized by phase (`AST -> Semantic -> HIR -> Frame Allocation -> MIR -> CIL`).
- `build-system-phase1.md`: Build system phase design notes.
- `cli-interface.md`: CLI behavior and interface notes.
- `language-server.md`: Language Server design notes.
- `semantic-phase-design.md`: Semantic phase design details.
- `unit-void-design.md`: Unit/Void semantics design notes.

## Usage
- Keep `PLANS.md` focused on active execution plans.
- Keep durable design rationale and historical records in this `notes/` directory.
- Add new compiler design decisions to `notes/phases/` in the corresponding phase file.
