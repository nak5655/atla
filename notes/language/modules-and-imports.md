# Modules and Imports

## Source paths

- `src/Atla.Core/Syntax/Data/Ast.fs`
- `src/Atla.Core/Semantics/Resolve.fs`
- `src/Atla.Core/DependencyLoader.fs`
- `src/Atla.Build/Resolver.fs`

## Current policy

Module and import semantics are resolved before semantic analysis consumes declarations. Build-level dependency resolution is documented in `notes/build/`; compiler-side dependency loading is documented in `notes/compiler/dependencies/`.
