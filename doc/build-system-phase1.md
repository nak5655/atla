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
