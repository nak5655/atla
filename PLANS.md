# Plan

- [x] `Mir.Method` と `Mir.Type` の定義に `name` メンバを追加する。
- [x] `Mir.Method` / `Mir.Type` のコンストラクタ引数を更新し、生成箇所（Layout）を追従させる。
- [x] `Layout` で `Hir.Module.scope` から `SymbolId` に対応する名前を解決して MIR 名に設定する。
- [x] 全テストを実行して影響範囲を確認する。

## 2026-04-08 Gen TypeId 変換対応

- [x] Genモジュールの実装方針を整理し、`TypeId -> System.Type` 変換を追加する計画を明記する。
- [x] Genモジュールに `SymbolId -> TypeBuilder` の変換テーブルを利用する `TypeId -> System.Type` 変換関数を実装する。
- [x] メソッド/コンストラクタ/フィールド生成で上記変換関数を利用するように差し替える。
- [x] Genに対するテストを追加・更新し、型解決の回帰を防止する。
- [x] テストスイートを実行して変更の妥当性を確認する（既存テスト不整合により一部失敗を確認）。
