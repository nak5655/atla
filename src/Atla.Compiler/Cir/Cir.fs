namespace Atla.Compiler.Cir

open System.Reflection
open System.Reflection.Emit
open Atla.Compiler.Types

module Cir =
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

    type Ins =
        | LdLoc of index: int
        | StLoc of index: int
        | LdArg of index: int
        | StArg of index: int
        | LdLocA of index: int
        | LdArgA of index: int
        | LdI32 of value: int
        | LdF64 of value: float
        | LdStr of str: string
        | StFld of fieldInfo: FieldInfo
        | LdFld of fieldInfo: FieldInfo
        | Add
        | Sub
        | Mul
        | Div
        | Rem
        | Or
        | And
        | Eq
        | Call of method: Choice<MethodInfo, ConstructorInfo>
        | CallVirt of method: MethodInfo
        | NewObj of ctor: ConstructorInfo
        | Ret
        | BeginExceptionBlock
        | BeginFinallyBlock
        | EndExceptionBlock
        | Nop
        | MarkLabel of label: Label
        | Br of label: Label // jump
        | BrTrue of label: Label
        | BrFalse of label: Label

    type Method(name: string, args: System.Type list, ret: System.Type, frame: Frame, body: Ins list) =
        member this.name = name
        member this.args = args
        member this.ret = ret
        member this.frame = frame
        member this.body = body

    type Constructor(args: System.Type list, frame: Frame, body: Ins list) =
        member this.args = args
        member this.frame = frame
        member this.body = body

    type Type(name: string, ctors: Constructor list, methods: Method list) =
        member this.name = name
        member this.ctors = ctors
        member this.methods = methods

    type Module(name: string, types: Type list, methods: Method list) =
        member this.name = name
        member this.types = types
        member this.methods = methods

    type Assembly(name: string, modules: Module list) =
        member this.name = name
        member this.modules = modules
