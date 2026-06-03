# Document Sync

## Source paths

- `src/Atla.LanguageServer/Server.fs`
- `src/Atla.LanguageServer.Tests/IncrementalSyncTests.fs`

## Current policy

Document sync owns in-memory text state and incremental/full text change application before diagnostics and semantic features are recomputed.
