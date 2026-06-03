# Notes

Implementation and design notes for Atla.

## Sections

- [`language/`](language/README.md) — Atla language semantics and user-visible behavior.
- [`compiler/`](compiler/README.md) — `Atla.Core` compiler pipeline, IRs, dependency loading, and code generation.
- [`build/`](build/README.md) — `Atla.Build` project configuration, dependency resolution, build flow, package kinds, and install flow.
- [`cli/`](cli/README.md) — `Atla.Console` command behavior and user-facing output.
- [`language-server/`](language-server/README.md) — `Atla.LanguageServer` LSP behavior.
- [`vscode/`](vscode/README.md) — VSCode extension integration.

## Policy

- Keep notes grouped by feature unit rather than by historical implementation phase.
- Prefer current implementation facts over old phase plans.
- If a note describes implementation behavior, list the corresponding source paths near the top of the note.
- Do not add unrelated sample, decision-log, standard-library, or catch-all historical buckets under `notes/`.
