# Package Types

## Source paths

- `src/Atla.Build/Build.fs`
- `src/Atla.Build/Resolver.fs`
- `src/Atla.Core/AtlaLib.fs`
- `src/Atla.Core/DependencyLoader.fs`

## Current policy

Package type behavior is build-facing. `.atlalib` container and symbol metadata details are documented in `notes/compiler/dependencies/atlalib.md`.

## Known package kinds

- `lib` — Atla library package intended for import through compiler dependency metadata.
- `dll` — .NET assembly output intended for executable or assembly-level consumption.
