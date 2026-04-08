# Plan

- [x] `Mir.Method` / `Mir.Constructor` の現在実装と利用箇所を調査する。
- [x] `Mir.Method` と `Mir.Constructor` に `frame` メンバを追加する。
- [x] `Layout` で生成したフレームを `Mir.Method` / `Mir.Constructor` に保持するよう修正する。
- [x] 影響を受ける呼び出し箇所・テストを更新する。
- [x] 検証コマンドを実行する（既存の `Scope`/`Gen` 周辺エラーでビルド失敗を確認）。
