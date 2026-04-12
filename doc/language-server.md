# Language Server Design Notes

## Phase 3: Document sync and diagnostics stabilization (2026-04-12)

This phase fixes document lifecycle determinism and diagnostics delivery behavior.

### Policy decisions

1. Document lifecycle state transitions are centralized in `Server`.
   - `didOpen` creates/updates an in-memory buffer.
   - `didChange` overwrites the full-text buffer (Full sync mode).
   - `didClose` must publish empty diagnostics and then release the buffer.

2. URI normalization is mandatory before using a document key.
   - Internal keys normalize `file://` URIs with full paths.
   - Compilation targets are deterministic: only `file://` documents are compilable.
   - If workspace roots are provided by initialize params, only files under those roots are compiled.

3. Diagnostics conversion must pass through a mapping layer.
   - Compiler errors are transformed into intermediate diagnostics and then to LSP diagnostics.
   - This keeps room for future lex/parse/semantic split without changing request handlers.

4. Successful compilation must always send empty diagnostics (`[]`) to clear stale errors.

## Phase 4: Diagnostics quality improvements (2026-04-12)

1. `LSPTypes.Diagnostic` includes optional `severity`, `source`, and `code`.
2. Diagnostics prioritize extractable spans from compiler messages (`at line:col` / `Line = ; Column =`) and only fall back to `Span.Empty` when unavailable.
3. Diagnostics ordering is deterministic (range, message, stable insertion index).
4. Snapshot-style tests cover semantic unresolved identifier, semantic type mismatch, and syntax-error paths.
