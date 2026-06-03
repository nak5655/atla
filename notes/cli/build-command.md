# Build Command

## Source paths

- `src/Atla.Console/Program.fs`
- `src/Atla.Build/Build.fs`

## Current policy

The CLI build command parses user arguments, delegates project build behavior to `Atla.Build`, and formats user-facing status and diagnostics.
