# Atla CLI

`Atla.Console` is the executable frontend for `Atla.Core` + `Atla.Build`.

## Usage

```bash
dotnet run --project src/Atla.Console -- build <projectRoot> [-o <outDir>] [--name <assemblyName>]
# publish 後: atla.exe build <projectRoot> [-o <outDir>] [--name <assemblyName>]
```

## Project layout

`build` コマンドは `projectRoot` を起点に以下を読み取ります。

- `atla.toml`
- `src/main.atla`

最小 `atla.toml`:

```toml
[package]
name = "hello"
version = "0.1.0"
```

依存定義（ローカル path）:

```toml
[dependencies]
corelib = { path = "../corelib" }
utils = "../utils"
```

## Behavior

- `projectRoot` は存在するディレクトリである必要があります。
- `atla.toml` は必須です。
- エントリポイントは `src/main.atla` 固定です。
- 出力ディレクトリの既定値は `<projectRoot>/out` です。
- アセンブリ名の既定値は `atla.toml` の `package.name` です。
- 依存解決時に以下を診断します。
  - 欠損パス
  - 循環依存
  - 重複パッケージ名
- Exit code は成功時 `0`、失敗時 `1` です。
- Published executable name is `atla.exe`.

## Test project

- CLI tests live in `src/Atla.Console.Tests`.

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
