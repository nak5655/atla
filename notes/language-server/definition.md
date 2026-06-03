# Definition Lookup

## Source paths

- `src/Atla.LanguageServer/Server.fs`
- `src/Atla.LanguageServer.Tests/IntelliSenseTests.fs`
- `src/Atla.Core/Semantics/PositionIndex.fs`

## Current policy

Definition lookup maps a cursor position to a semantic symbol, then returns the declaration span if one is known.
