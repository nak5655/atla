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

    type BuiltinFunctions =
        | Array
        /// 単精度（Float / float32）への変換組込関数 `toFloat`。
        | ToFloat
        /// 倍精度（Double / float）への変換組込関数 `toDouble`。
        | ToDouble
        | ToInt
