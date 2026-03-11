namespace Atla.Compiler.Cir

open System.Reflection
open System.Reflection.Emit
open Atla.Compiler.Types

module Cir =
    type Label = {
        label: System.Reflection.Emit.Label option
        ilOffset: int
    }

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

    type Method = {
        frame: Frame
        body: Ins list
        builder: MethodBuilder
    }

    type Constructor = {
        frame: Frame
        body: Ins list
        builder: ConstructorBuilder
    }

    type Type = {
        ctors: ConstructorInfo list
        methods: MethodInfo list
        builder: TypeBuilder
    }

    type Module = {
        name: string
        methods: Method list
        types: Type list
        builder: ModuleBuilder
    }

    type Assembly = {
        name: string
        modules: Module list
        builder: AssemblyBuilder
    }