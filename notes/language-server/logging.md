# Language Server Logging

## Source paths

- `src/Atla.LanguageServer/Log.fs`
- `src/Atla.LanguageServer/Server.fs`

## Current policy

Logging must be safe for stdio-based LSP operation and must not corrupt JSON-RPC output.
