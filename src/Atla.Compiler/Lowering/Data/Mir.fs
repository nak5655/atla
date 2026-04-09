namespace Atla.Compiler.Lowering.Data

open System
open System.Reflection
open System.Reflection.Emit
open System.Collections.Generic
open Atla.Compiler.Semantics.Data

// MIRでは
// - 型はTypeMetaを除去済み
// - 変数名をインデックスに変換済み
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

    type Reg =
        | Loc of int
        | Arg of int
        override this.ToString() =
            match this with
            | Loc index -> sprintf "Loc(%d)" index
            | Arg index -> sprintf "Arg(%d)" index

    type Frame() =
        let mutable _args: Dictionary<SymbolId, Reg> = Dictionary()
        let mutable _locs: Dictionary<SymbolId, Reg> = Dictionary()
        member this.args = _args
        member this.locs = _locs

        member this.addArg(sid: SymbolId): Reg =
            let reg = Reg.Arg(_args.Count)
            _args.Add(sid, reg)
            reg

        member this.addLoc(sid: SymbolId): Reg =
            let reg = Reg.Loc(_locs.Count)
            _locs.Add(sid, reg)
            reg

        member this.get(sid: SymbolId): Reg option =
            if _args.ContainsKey(sid) then Some _args.[sid]
            elif _locs.ContainsKey(sid) then Some _locs.[sid]
            else None

    // Values in MIR
    type Value =
        | ImmVal of Imm
        | RegVal of Reg
        | FieldVal of field: FieldInfo
        | MethodVal of method: MethodInfo
        override this.ToString() =
            match this with
            | ImmVal v -> sprintf "Imm(%s)" (v.ToString())
            | RegVal v -> sprintf "Reg(%s)" (v.ToString())
            | FieldVal (fi) -> sprintf "Field(%A)" fi
            | MethodVal (mi) -> sprintf "Method(%A)" mi

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
        let mutable _label: System.Reflection.Emit.Label option = None
        let mutable _ilOffset: int = -1

        member this.label
            with get() = _label
            and set(value) = _label <- value

        member this.ilOffset
            with get() = _ilOffset
            and set(value) = _ilOffset <- value

        member this.get(gen: ILGenerator): System.Reflection.Emit.Label =
            if _label.IsNone then
                _label <- Some (gen.DefineLabel())
            _label.Value

    // Instructions
    type Ins =
        | Assign of dest: Reg * value: Value
        | AssignField of inst: Reg * field: FieldInfo * value: Value
        | TAC of dest: Reg * lhs: Value * op: OpCode * rhs: Value
        | Call of method: Choice<MethodInfo, ConstructorInfo> * args: Value list
        | CallAssign of dst: Reg * method: MethodInfo * args: Value list
        | New of dst: Reg * ctor: ConstructorInfo * args: Value list
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
    type Field(sid: SymbolId, tid: TypeId) =
        let mutable _builder: FieldBuilder option = None
        member this.sym = sid
        member this.typ = tid
        member this.builder
            with get() = _builder.Value
            and set(v) = _builder <- Some v

    type Constructor(args: TypeId list, body: Ins list, frame: Frame) =
        let mutable _builder: ConstructorBuilder option = None
        member this.args = args
        member this.body = body
        member this.frame = frame
        member this.builder
            with get() = _builder.Value
            and set(v) = _builder <- Some v

    type Method(name: string, sid: SymbolId, args: TypeId list, ret: TypeId, body: Ins list, frame: Frame) =
        let mutable _builder: MethodBuilder option = None
        member this.name = name
        member this.sym = sid
        member this.args = args
        member this.ret = ret
        member this.body = body
        member this.frame = frame
        member this.builder
            with get() = _builder.Value
            and set(v) = _builder <- Some v

    type Type(name: string, sid: SymbolId, fields: Field list, ctors: Constructor list, methods: Method list) =
        let mutable _builder: TypeBuilder option = None
        member this.name = name
        member this.sym = sid
        member this.fields = fields
        member this.ctors = ctors
        member this.methods = methods
        member this.builder
            with get() = _builder.Value
            and set(v) = _builder <- Some v

    type Module(name: string, types: Type list, methods: Method list) =
        let mutable _builder: ModuleBuilder option = None
        member this.name = name
        member this.types = types
        member this.methods = methods
        member this.builder
            with get() = _builder.Value
            and set(v) = _builder <- Some v

    type Assembly(name: string, modules: Module list) =
        let mutable _builder: PersistedAssemblyBuilder option = None
        member this.name = name
        member this.modules = modules
        member this.builder
            with get() = _builder.Value
            and set(v) = _builder <- Some v
