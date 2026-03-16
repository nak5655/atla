namespace Atla.Compiler.Lowering

open Atla.Compiler.Types
open Atla.Compiler.Mir
open Atla.Compiler.Cir

module Linearize =
    let rec linearizeValue (value: Mir.Value) : Cir.Ins list * Mir.Value =
        match value with
        | Mir.Value.ImmVal imm ->
            let ins = 
                match imm with
                | Mir.Imm.Bool b -> [Cir.Ins.LdI32(if b then 1 else 0)]
                | Mir.Imm.Int i -> [Cir.Ins.LdI32 i]
                | Mir.Imm.Float f -> [Cir.Ins.LdF64 f]
                | Mir.Imm.String s -> [Cir.Ins.LdStr s]
            (ins, value)
        | Mir.Value.Loc index ->
            ([Cir.Ins.LdLoc index], value)
        | Mir.Value.Arg index ->
            ([Cir.Ins.LdArg index], value)
        | Mir.Value.Field (inst, field) ->
            let (instIns, instVal) = linearizeValue (Mir.Value.Arg 0) // Assuming 'this' is at Arg 0
            let ins = instIns @ [Cir.Ins.LdFld field]
            (ins, value)

    let linealizeIns (frame: Frame) (mirIns: Mir.Ins) : Cir.Ins list =
        let mutable ins = []
        match mirIns with
        | Mir.Ins.Assign (sym, value) ->
            let (valueIns, valueVal) = linearizeValue value
            match frame.resolve(sym) with
            | Some (FramePosition.Arg index) ->
                ins <- ins @ valueIns @ [Cir.Ins.StArg index]
            | Some (FramePosition.Loc index) ->
                ins <- ins @ valueIns @ [Cir.Ins.StLoc index]
            | None -> failwithf "Undefined variable: %A" sym
        | Mir.Ins.RetValue value ->
            let (valueIns, _) = linearizeValue value
            ins <- ins @ valueIns @ [Cir.Ins.Ret]
        | Mir.Ins.Ret ->
            ins <- ins @ [Cir.Ins.Ret]
        | _ -> failwithf "Unsupported instruction: %A" mirIns
        ins

    let linearizeMethod (mirMethod: Mir.Method) : Cir.Method =
        let ins = List.collect (linealizeIns mirMethod.frame) mirMethod.body

        let args = List.map (fun (arg: Mir.Argum) -> arg.typ) mirMethod.args
        Cir.Method(mirMethod.name, args, mirMethod.ret, mirMethod.frame, ins)

    let linearizeType (mirType: Mir.Type) : Cir.Type =
        let methods = List.map linearizeMethod mirType.methods
        Cir.Type(mirType.name, [], methods)

    let linearizeModule (mirModule: Mir.Module) : Cir.Module =
        let linearizedMethods = List.map linearizeMethod mirModule.methods
        let linearizedTypes = List.map linearizeType mirModule.types
        Cir.Module(mirModule.name, linearizedTypes, linearizedMethods)

    let linearizeAssembly (mirAssembly: Mir.Assembly) : Cir.Assembly =
        let linearizedModules = List.map linearizeModule mirAssembly.modules
        Cir.Assembly(mirAssembly.name, linearizedModules)