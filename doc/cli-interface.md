# Atla CLI (minimal)

`Atla.Cli` is the executable frontend for `Atla.Compiler`.

## Usage

```bash
dotnet run --project src/Atla.Cli -- build <input.atla> [-o <outDir>] [--name <assemblyName>]
```

## Behavior

- Input must be an existing `.atla` file.
- Output directory defaults to `./out`.
- Assembly name defaults to the input filename without extension.
- Exit code is `0` on success and `1` on failure.


## Test project

- CLI tests live in `src/Atla.Cli.Tests`.
