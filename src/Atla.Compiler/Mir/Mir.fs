namespace Atla.Compiler.Mir

open System
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic
open Atla.Compiler.Types

module Mir =
    // Immediate values in MIR
    type Imm =
        | Bool of bool
        | Int of int
        | Float of float
        | String of string
        override this.ToString() =
            match this with
            | Bool v -> sprintf "Bool(%b)" v
            | Int v -> sprintf "Int(%d)" v
            | Float v -> sprintf "Float(%f)" v
            | String s -> sprintf "String(%s)" s

    // Values in MIR
    type Value =
        | ImmVal of Imm
        | Loc of int
        | Arg of int
        | Field of inst: Symbol * field: FieldInfo
        override this.ToString() =
            match this with
            | ImmVal v -> sprintf "Imm(%s)" (v.ToString())
            | Loc s -> sprintf "Loc(%A)" s
            | Arg s -> sprintf "Arg(%A)" s
            | Field (sym, fi) -> sprintf "Field(%A, %A)" sym fi

    type OpCode =
        | Add
        | Sub
        | Mul
        | Div
        | Mod
        | Or
        | And
        | Eq

    type Label() =
        override this.ToString() = sprintf "Label(%d)" (this.GetHashCode())

    // Instructions
    type Ins =
        | Assign of name: Symbol * value: Value
        | AssignField of inst: Symbol * field: FieldInfo * value: Value
        | TAC of dest: Symbol * lhs: Value * op: OpCode * rhs: Value
        | Call of method: Choice<MethodInfo, ConstructorInfo> * args: Value list
        | CallAssign of dst: Symbol * method: MethodInfo * args: Value list
        | New of dst: Symbol * ctor: ConstructorInfo * args: Value list
        | Ret
        | RetValue of value: Value
        | Jump of label: Label
        | JumpTrue of value: Value * label: Label
        | JumpFalse of value: Value * label: Label
        | MarkLabel of label: Label
        | Try of body: Ins list * finallyBody: Ins list
        override this.ToString() =
            match this with
            | Assign(name, value) -> sprintf "%A = %s" name (value.ToString())
            | AssignField(inst, field, value) -> sprintf "%A.%s = %s" inst field.Name (value.ToString())
            | TAC(dest, lhs, op, rhs) -> sprintf "%A = %s %A %s" dest (lhs.ToString()) op (rhs.ToString())
            | Call(method, args) -> sprintf "%A(%s)" method (String.Join(", ", args |> List.map (fun a -> a.ToString())))
            | CallAssign(dst, method, args) -> sprintf "%A = %A(%s)" dst method (String.Join(", ", args |> List.map (fun a -> a.ToString())))
            | New(dst, ctor, args) -> sprintf "%A = %A(%s)" dst ctor (String.Join(", ", args |> List.map (fun a -> a.ToString())))
            | Ret -> "Ret"
            | RetValue v -> sprintf "return %s" (v.ToString())
            | Jump label -> sprintf "Jump %s" (label.ToString())
            | JumpTrue(v, label) -> sprintf "JumpTrue %s %s" (v.ToString()) (label.ToString())
            | JumpFalse(v, label) -> sprintf "JumpFalse %s %s" (v.ToString()) (label.ToString())
            | MarkLabel label -> sprintf "MarkLabel %s" (label.ToString())
            | Try(body, finallyBody) -> sprintf "Try(body=%d, finally=%d)" (List.length body) (List.length finallyBody)

    // Convenience wrapper for fields in generated types
    type Field(name: string, typ: System.Type) =
        member this.name = name
        member this.typ = typ

    type Argum(name: string, typ: System.Type) =
        member this.name = name
        member this.typ = typ

    type Constructor(args: Argum list, body: Ins list, frame: Frame) =
        member this.args = args
        member this.body = body
        member this.frame = frame

    type Method(name: string, args: Argum list, ret: System.Type, body: Ins list, frame: Frame) =
        member this.name = name
        member this.args = args
        member this.ret = ret
        member this.body = body
        member this.frame = frame

    type Type(name: string, fields: Field list, ctors: Constructor list, methods: Method list) =
        member this.name = name
        member this.fields = fields
        member this.ctors = ctors
        member this.methods = methods

    type Module(name: string, types: Type list, methods: Method list) =
        member this.name = name
        member this.types = types
        member this.methods = methods

    type Assembly(name: string, modules: Module list) =
        member this.name = name
        member this.modules = modules
