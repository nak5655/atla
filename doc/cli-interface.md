# Atla CLI (minimal)

`Atla.Cli` is the executable frontend for `Atla.Compiler`.

## Usage

```bash
dotnet run --project src/Atla.Cli -- build <input.atla> [-o <outDir>] [--name <assemblyName>]
# publish 後: atla.exe build <input.atla> [-o <outDir>] [--name <assemblyName>]
```

## Behavior

- Input must be an existing `.atla` file.
- Output directory defaults to `./out`.
- Assembly name defaults to the input filename without extension.
- Exit code is `0` on success and `1` on failure.
- Published executable name is `atla.exe`.


## Test project

- CLI tests live in `src/Atla.Cli.Tests`.

## Self-contained single-file publish

`Atla.Console` / `Atla.LanguageServer` はプロジェクト既定で `PublishSelfContained=true` + `PublishSingleFile=true` を有効化しています。

```bash
# Atla.Console (Windows x64 単一exe)
dotnet publish src/Atla.Console/Atla.Console.fsproj -c Release -r win-x64
.\src\Atla.Console\bin\Release\net10.0\win-x64\publish\atla.exe --help

# Atla.LanguageServer (Windows x64 単一exe)
dotnet publish src/Atla.LanguageServer/Atla.LanguageServer.fsproj -c Release -r win-x64
.\src\Atla.LanguageServer\bin\Release\net10.0\win-x64\publish\atla-lsp.exe
```

> Runtime Identifier (`-r`) は配布先OS/CPUに合わせて指定してください。
