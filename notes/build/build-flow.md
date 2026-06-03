# Build Flow

## Source paths

- `src/Atla.Build/Build.fs`
- `src/Atla.Core/Compile.fs`
- `src/Atla.Console/Program.fs`

## Current policy

Build flow bridges project configuration and compiler invocation. CLI command parsing stays in `notes/cli/`; compiler phase behavior stays in `notes/compiler/`.
