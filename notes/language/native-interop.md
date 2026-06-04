# Native Interop

## Source paths

- `src/Atla.Core/Semantics/NativeInterop.fs`
- `src/Atla.Core/Semantics/ExprAnalyze.fs`
- `src/Atla.Core/Lowering/Gen.fs`

## Current policy

Native interop notes cover Atla references to .NET types, constructors, methods, properties, overload selection, and optional argument behavior.

## .NET 基底クラス継承

`struct T: DotNetClass` 構文を使って .NET クラスを継承できる。

```atla
import System'Exception
struct MyError: Exception
    val code: Int
impl MyError
    fn new (code: Int): MyError
        { code = code } MyError.
    override fn ToString self: String
        base'ToString.
```

### 制約
- `X` は .NET クラスでなければならない（`X.IsClass`）
- interface の継承は不可（`X.IsInterface` → エラー）
- sealed クラスの継承は不可（`X.IsSealed` → エラー）
- `override` は native base をもつ型の `impl T` ブロック内のみ有効
- `base` はインスタンスメソッド内のみ使用可能

### `struct T: X` の X 解決順序
1. 同一モジュール内 union → union バリアント扱い（既存動作）
2. import 済み .NET class → native base 扱い（.NET 継承）
3. それ以外 → `"Undefined base type"` エラー

X が union かつ .NET class として両方解決できる場合 → 曖昧エラー
