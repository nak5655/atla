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
        | Sym of Symbol
        | Addr of Symbol
        | Field of inst: Symbol * field: FieldInfo
        override this.ToString() =
            match this with
            | ImmVal v -> sprintf "Imm(%s)" (v.ToString())
            | Sym s -> sprintf "Sym(%A)" s
            | Addr s -> sprintf "Addr(%A)" s
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
    type FieldDef(name: string, tyBuilder: TypeBuilder, fieldType: System.Type) =
        let builder = tyBuilder.DefineField(name, fieldType, FieldAttributes.Public)
        member this.name = name
        member this.builder = builder

    type ConstructorDef(builder: ConstructorBuilder) =
        let mutable body : Ins list = []
        member this.builder = builder
        member val frame = Unchecked.defaultof<obj> with get, set // placeholder for Frame
        member this.Body
            with get() = body
            and set(v) = body <- v

    type MethodDef(builder: MethodBuilder) =
        let mutable body : Ins list = []
        member this.builder = builder
        member val frame = Unchecked.defaultof<obj> with get, set
        member this.Body
            with get() = body
            and set(v) = body <- v

    type MethodContainer =
        abstract member DefineMethod: name:string * args:System.Type list * ret:System.Type -> MethodDef

    type TypeDef(modBuilder: ModuleBuilder, name: string) =
        let builder = modBuilder.DefineType(name, TypeAttributes.Public, typeof<System.Object>)
        let fields = List<FieldDef>()
        let ctors = List<ConstructorDef>()
        let methods = List<MethodDef>()
        member this.builder = builder
        member this.fields = fields
        member this.ctors = ctors
        member this.methods = methods
        member this.DefineField(name: string, fieldType: System.Type) =
            let f = FieldDef(name, builder, fieldType)
            fields.Add(f)
            f
        member this.DefineConstructor(argTypes: System.Type seq) =
            let ctorBuilder = builder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, argTypes |> Seq.toArray)
            let c = ConstructorDef(ctorBuilder)
            ctors.Add(c)
            c
        interface MethodContainer with
            member this.DefineMethod(name, args, ret) =
                let mb = builder.DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.Static, ret, args |> List.toArray)
                let m = MethodDef(mb)
                methods.Add(m)
                m

    type ModuleDef(name: string) =
        let methods = List<MethodDef>()
        member this.DefineMethod(name: string, args: System.Type list, ret: System.Type) =
            let mb = builder.DefineGlobalMethod(name, MethodAttributes.Public ||| MethodAttributes.Static, ret, args |> List.toArray)
            let m = MethodDef(mb)
            methods.Add(m)
            m

    type Assembly(name: string) =
        member this.name = name
        member val modules = Dictionary<string, ModuleDef>()
