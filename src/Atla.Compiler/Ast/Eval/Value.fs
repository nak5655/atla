namespace Atla.Compiler.Ast.Eval

type Value =
    | Unit
    | Int of int
    | Float of float
    | String of string
    | Function of (Value list -> Value)
    | Data of Map<string, Value>