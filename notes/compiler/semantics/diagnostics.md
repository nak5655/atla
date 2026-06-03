# Semantic Diagnostics

## Source paths

- `src/Atla.Core/Semantics/Data/Diagnostic.fs`
- `src/Atla.Core/Semantics/Data/PhaseResult.fs`
- `src/Atla.Core/Semantics/Analyze.fs`
- `src/Atla.LanguageServer/Server.fs`

## Current policy

Compiler diagnostics are collected as semantic phase results and later converted to CLI or LSP output by the caller.
