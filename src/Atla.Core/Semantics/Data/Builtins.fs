namespace Atla.Core.Semantics.Data

module Builtins =
    type Operators =
        | OpAdd
        | OpSub
        | OpMul
        | OpDiv
        | OpMod
        | OpEq
        | OpNe
        | OpAnd
        | OpOr
        | OpNeg
        | OpLt
        | OpGt
        | OpLe
        | OpGe

    type BuiltinFunctions =
        | Array
        /// 空の `System.Collections.Generic.List<T>` を構築する組込関数 `List`。
        /// 要素型 T は呼び出し文脈（期待型）から推論する。
        | List
        /// 単精度（Float / float32）への変換組込関数 `toFloat`。
        | ToFloat
        /// 倍精度（Double / float）への変換組込関数 `toDouble`。
        | ToDouble
        | ToInt
        /// 整数範囲を生成する組込関数 `range`。`start end range` で start..end（終端を含まない）の
        /// `IEnumerable<int>` を返す。内部で個数 = end - start を計算し Enumerable.Range へ下す。
        | Range
