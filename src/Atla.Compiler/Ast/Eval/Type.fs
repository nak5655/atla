namespace Atla.Compiler.Ast.Eval

type Type =
    | Unit
    | Int
    | Float
    | String
    | Data of Map<string, Type>
    | Native of System.Type