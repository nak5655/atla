# LSP Diagnostics

## Source paths

- `src/Atla.LanguageServer/Server.fs`
- `src/Atla.LanguageServer.Tests/DiagnosticsTests.fs`
- `src/Atla.Core/Semantics/Data/Diagnostic.fs`

## Current policy

LSP diagnostics convert compiler diagnostics to LSP ranges and severities and publish them per document URI.
