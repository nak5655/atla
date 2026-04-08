# Plan

- [x] `Scope` の `types` を `Dictionary<string, TypeId>` に変更し、関連 API (`DeclareType` / `ResolveType`) を追従する。
- [x] `Scope.GlobalScope` を削除し、組み込み型の初期化を `Analyze` 側へ移す。
- [x] GlobalScope で定義していた組み込み関数を `SymbolTable` で定義し、モジュールスコープへ登録する。
- [x] テストを実行して変更を検証する。
