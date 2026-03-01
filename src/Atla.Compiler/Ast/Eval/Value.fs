namespace Atla.Compiler.Ast.Eval

type Value =
    | Unit
    | Int of int
    | Float of float
    | String of string
    | Data of Map<string, Value>