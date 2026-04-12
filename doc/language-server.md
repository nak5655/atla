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
