# Atla.Build フェーズ1 設計確定

更新日: 2026-04-15

## 目的

`Atla.Build` プロジェクト追加に向けたフェーズ1として、manifest最小仕様・責務境界・依存受け渡しモデルを確定する。

## atla.toml 最小仕様（フェーズ1）

フェーズ1では `atla.toml` の対象を `[package]` セクションのみに限定する。

```toml
[package]
name = "hello"
version = "0.1.0"
```

- `name` は必須
- `version` は必須

`dependencies` は将来フェーズで導入し、NuGetパッケージ解決へ対応する。

2026-04-15 時点のフェーズ0-3合意:

- `path` 指定がある場合は path 依存として扱う（優先度: `path > version`）。
- `path` がなく `version` がある場合は NuGet 依存として扱う。
- `path` と `version` の同時指定は不正とする。
- NuGet依存は `NUGET_PACKAGES`（未設定時 `~/.nuget/packages`）配下から解決する。
- NuGet依存の `ResolvedDependency.source` は実体ディレクトリの絶対パスとする。
- 実装は `Build.fs`（manifest解析）と `Resolver.fs`（依存解決）に責務分離する。
- 競合解決は厳密一致とし、同一依存名は `version` が一致する場合のみ統合する。
- キャッシュ不在時は既定で失敗し、`ATLA_BUILD_ENABLE_NUGET_RESTORE=1` で自動 restore 試行を有効化できる。
- テストは `BuildTests`（build経路）と `ResolverTests`（NuGet/競合解決）へ分割する。
- 決定性保証として、依存出力順序と診断順序の再現性をテストで検証する。

```toml
[dependencies]
"Newtonsoft.Json" = { version = "13.0.3" } # nuget
local-lib = { path = "../local-lib" }     # local path
```

## 責務境界

- `Atla.Build`
  - プロジェクトルート決定
  - `atla.toml` 読み取り/検証
  - 依存解決（将来 `dependencies` + NuGet を含む）
- `Atla.Core`
  - コンパイルパイプライン（AST -> Semantic Analysis -> HIR -> Frame Allocation -> MIR -> CIL）
  - `Atla.Build` から受け取る解決済み依存情報を入力として受理

## 依存受け渡しモデル

`Atla.Core.Compile` へは `ResolvedDependency list` を渡す方針を採用する。

このフェーズでは型詳細は固定せず、次フェーズで `Atla.Build` 実装と同時に具体化する。
