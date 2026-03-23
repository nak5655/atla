namespace Atla.Compiler.Hir

open System.Reflection
open Atla.Compiler.Mir

type Symbol =
    | Arg of typ: TypeCray
    | Local of typ: TypeCray
    | Method of typ: TypeCray
    | NativeMethod of typ: TypeCray * methodInfo: System.Reflection.MethodInfo
    | BuildinMethod of typ: TypeCray * method: (Mir.Reg * Mir.Value * Mir.Value -> Mir.Ins list)

    override this.ToString() =
        match this with
        | Arg typ -> sprintf "Arg(%A)" typ
        | Local typ -> sprintf "Local(%A)" typ
        | Method typ -> sprintf "Method(%A)" typ
        | NativeMethod (typ, methodInfo) -> sprintf "NativeMethod(%A, %s)" typ methodInfo.Name
        | BuildinMethod (typ, _) -> sprintf "BuildinMethod(%A)" typ

    member this.typ =
        match this with
        | Arg typ -> typ
        | Local typ -> typ
        | Method typ -> typ
        | NativeMethod (typ, _) -> typ
        | BuildinMethod (typ, _) -> typ