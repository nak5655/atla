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
        | ToSingle
        | ToFloat
        | ToInt
