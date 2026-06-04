# `impl`

## Source paths

- `src/Atla.Core/Syntax/Data/Ast.fs`
- `src/Atla.Core/Semantics/Resolve.fs`
- `src/Atla.Core/Semantics/Analyze.fs`

## 構文

```
impl T [TypeParams...]
impl Role [TypeParams...] for T
```

- `impl T` — `data` または `enum` 型 T のメソッド実装ブロック。
- `impl Role for T` — T が Role（Atla の `role` 型または .NET インターフェイス）を実装することを宣言する。

## 制約

- 対象型は同一モジュール内の `data` または `enum` 型のみ。
- `impl` ブロック内には `fn` 宣言のみ許可。
- 同一型 T に対する `impl T`（`for` なし）は最大 1 つ。
- 同一組 `(Role, T)` に対する `impl Role for T` は最大 1 つ（Role が異なれば共存可）。
- `impl Role for T` の Role が .NET 型の場合、インターフェイスでなければならない。
- メソッド名の重複は不可。

## .NET 基底クラス継承

`struct T: DotNetClass` 構文については `native-interop.md` を参照。
