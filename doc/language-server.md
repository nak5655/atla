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

## Phase 5: Semantic Tokens precision improvements (2026-04-12)

1. Compatibility policy prioritizes current spec alignment over backward compatibility for semantic tokens.
2. Canonical token kinds are fixed to five types only:
   - `keyword`
   - `type`
   - `variable`
   - `number`
   - `string`
3. `InternalTokenize` emits only those five kinds; unclassified tokens are not sent.
4. Input normalization for semantic tokenization is deterministic:
   - UTF-8 BOM at head is ignored
   - CRLF / CR is normalized to LF before lexing
   - Equivalent content yields identical semantic token data
   - Token span width is preserved to the right edge (no rightmost-character truncation)
5. If client capability is empty or has only unknown token kinds:
   - server returns empty semantic token data
   - server sends `window/logMessage` with fallback reason
6. Semantic tokens snapshot validation fixes `resultId` to empty string and focuses verification on `data`.

## Phase 6: Test foundation and regression prevention (2026-04-12)

1. `Atla.LanguageServer.Tests` is split by responsibility:
   - `Message`
   - `ServerLifecycle`
   - `Diagnostics`
   - `SemanticTokens`
   - `Program`
2. E2E tests validate actual stdin/stdout framing using `Content-Length`.
3. Minimum normal E2E path is fixed to:
   - `initialize -> didOpen -> semanticTokens -> shutdown -> exit`
4. Minimum abnormal E2E set is fixed to:
   - malformed header
   - missing `Content-Length`
   - invalid JSON
   - unknown request
   - empty body
5. Required validation commands for LanguageServer changes:
   - `dotnet test src/Atla.LanguageServer.Tests/Atla.LanguageServer.Tests.fsproj`
   - `dotnet test src/Atla.slnx`

## Phase 7: Release readiness (2026-04-12)

### Supported LSP methods

- Requests
  - `initialize`
  - `shutdown`
  - `textDocument/semanticTokens/full`
- Notifications
  - `initialized`
  - `textDocument/didOpen`
  - `textDocument/didChange` (full sync)
  - `textDocument/didClose`
  - `exit`

### Unsupported methods

- Any request/notification not listed above is currently unsupported.
- Unknown requests return JSON-RPC `Method not found` (`-32601`).

### Known limitations and workarounds

1. Diagnostics
   - Limitation: stage split (lex/parse/semantic) is heuristic from message text.
   - Workaround: rely on compiler message text and span extraction for now.
2. Semantic tokens
   - Limitation: only five token kinds are emitted; no modifiers.
   - Workaround: client should gracefully handle empty/modifier-free legend.
3. Document sync
   - Limitation: full-sync (`didChange` full text) only.
   - Workaround: configure client to send full content changes.

### Recommended client settings

- Start command: `dotnet <path-to-atla-lsp.dll>`
- Transport: stdio
- Send full sync changes (`textDocumentSync.kind = Full` compatible behavior)
- Advertise semantic token types including:
  `keyword`, `type`, `variable`, `number`, `string`

### CI required checks policy

- Required checks:
  - `Atla.LanguageServer` build
  - `Atla.LanguageServer.Tests` test
- Merge is blocked when either required check fails.

### Phase 7 completion gate

Phase 7 is complete only when all of the following are true at the same time:
1. local full tests are green
2. E2E normal and abnormal tests are green
3. documentation updates are complete

## Build/Publish notes (2026-04-13)

- `Atla.LanguageServer` は `PublishSelfContained=true` + `PublishSingleFile=true` を有効化しています。
- `Atla.Console` も同様に単一実行ファイル publish を有効化しています。
- いずれも publish 時には Runtime Identifier (`-r`) を明示してください。

```bash
# Language Server: 単一exe
dotnet publish src/Atla.LanguageServer/Atla.LanguageServer.fsproj -c Release -r win-x64
.\src\Atla.LanguageServer\bin\Release\net10.0\win-x64\publish\atla-lsp.exe

# Console: 単一exe
dotnet publish src/Atla.Console/Atla.Console.fsproj -c Release -r win-x64
.\src\Atla.Console\bin\Release\net10.0\win-x64\publish\atla.exe --help
```

## Dependency injection on LSP compile route (2026-04-16)

1. `didOpen` / `didChange` compile flow now attempts project-root discovery by walking parent directories from the target document and finding the nearest `atla.yaml`.
2. If a manifest is found, `Atla.Build.BuildSystem.buildProject` is executed first and resolved `plan.dependencies` are injected into `Compiler.compile`.
3. If no manifest is found, the server falls back to compile with `dependencies = []`.
4. If the document is outside workspace roots, the server does not compile and publishes empty diagnostics (`[]`) to clear stale state deterministically.
5. LSP diagnostics source is stage-separated:
   - build failures: `source = "atla-build"`
   - compiler diagnostics: `source = "atla-compiler"`
