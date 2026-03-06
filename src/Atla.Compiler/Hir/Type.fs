namespace Atla.Compiler.Hir

type Type =
    | Unknown
    | CoverAll of Type list
    | Var of Type
    | Native of System.Type
    | Function of (Type list * Type)
    | Defined of Map<string, Type>