namespace Atla.Core.Tests.Semantics

open System.Runtime.CompilerServices

// セマンティクステスト専用の拡張メソッド定義。
[<Extension>]
type TestExtensions =
    // 非ジェネリックの最小拡張メソッドを提供する。
    [<Extension>]
    static member PlusTen(value: int) : unit =
        ignore value
