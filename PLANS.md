# Plan

- [x] `Frame` の定義位置と参照箇所（Layout/Gen/Tests/MIR）を確認する。
- [x] `Frame` 型を `Mir` モジュールへ移動する。
- [x] `Mir.Method` / `Mir.Constructor` の `frame` 型を `Mir.Frame` に変更する。
- [x] 参照側（Layout/Gen/Tests/fsproj）を `Mir.Frame` へ追従させる。
- [x] テストコマンドで検証する（既存の `Scope` / `Gen` 不整合で失敗）。
