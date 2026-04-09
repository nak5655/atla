namespace Atla.Compiler.Lowering

open Atla.Compiler.Semantics.Data
open Atla.Compiler.Lowering.Data

module Layout =
    type private KNormal = {
        ins: Mir.Ins list
        res: Mir.Value option
    }

    let private declareTemp (frame: Mir.Frame) : Mir.Reg =
        let sid = SymbolId(frame.locs.Count + frame.args.Count)
        frame.addLoc sid

    let rec private layoutExpr (frame: Mir.Frame) (expr: Hir.Expr) : KNormal =
        match expr with
        | Hir.Expr.Unit _ -> { ins = []; res = None }
        | Hir.Expr.Int (value, _) -> { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.Int value)) }
        | Hir.Expr.Float (value, _) -> { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.Float value)) }
        | Hir.Expr.String (value, _) -> { ins = []; res = Some(Mir.Value.ImmVal(Mir.Imm.String value)) }
        | Hir.Expr.Id (sid, _, _) ->
            match frame.get sid with
            | Some reg -> { ins = []; res = Some(Mir.Value.RegVal reg) }
            | None ->
                let argReg = frame.addArg sid
                { ins = []; res = Some(Mir.Value.RegVal argReg) }
        | Hir.Expr.MemberAccess (mem, _, _, _) ->
            match mem with
            | Hir.Member.NativeField fi -> { ins = []; res = Some(Mir.Value.FieldVal fi) }
            | Hir.Member.NativeMethod mi -> { ins = []; res = Some(Mir.Value.MethodVal mi) }
            | Hir.Member.NativeProperty pi ->
                match pi.GetMethod with
                | null -> failwithf "Property has no getter: %s" pi.Name
                | getter -> { ins = []; res = Some(Mir.Value.MethodVal getter) }
        | Hir.Expr.Call (func, _, args, tid, _) ->
            let argKn = args |> List.map (layoutExpr frame)
            let argIns = argKn |> List.collect (fun k -> k.ins)
            let argValues =
                argKn
                |> List.map (fun k ->
                    match k.res with
                    | Some value -> value
                    | None -> failwith "Argument expression did not produce a value")

            let emitCall (mi: System.Reflection.MethodInfo) =
                let returnsUnit = (mi.ReturnType = typeof<System.Void>)
                match tid, returnsUnit with
                | _, true
                | TypeId.Unit, _ ->
                    { ins = argIns @ [ Mir.Ins.Call(Choice1Of2 mi, argValues) ]; res = None }
                | _ ->
                    let dst = declareTemp frame
                    { ins = argIns @ [ Mir.Ins.CallAssign(dst, mi, argValues) ]; res = Some(Mir.Value.RegVal dst) }

            match func with
            | Hir.Callable.NativeMethod mi -> emitCall mi
            | Hir.Callable.NativeMethodGroup methods ->
                match methods |> List.tryFind (fun m -> m.GetParameters().Length = argValues.Length) with
                | Some mi -> emitCall mi
                | None -> failwithf "No overload matched argument count %d" argValues.Length
            | Hir.Callable.NativeConstructor ctor ->
                let dst = declareTemp frame
                { ins = argIns @ [ Mir.Ins.New(dst, ctor, argValues) ]; res = Some(Mir.Value.RegVal dst) }
            | Hir.Callable.NativeConstructorGroup ctors ->
                match ctors |> List.tryFind (fun c -> c.GetParameters().Length = argValues.Length) with
                | Some ctor ->
                    let dst = declareTemp frame
                    { ins = argIns @ [ Mir.Ins.New(dst, ctor, argValues) ]; res = Some(Mir.Value.RegVal dst) }
                | None -> failwithf "No constructor matched argument count %d" argValues.Length
            | Hir.Callable.BuiltinOperator op ->
                let dst = declareTemp frame
                let lhs = argValues |> List.tryItem 0 |> Option.defaultWith (fun () -> failwith "Missing lhs")
                let rhs = argValues |> List.tryItem 1 |> Option.defaultWith (fun () -> failwith "Missing rhs")
                let opcode =
                    match op with
                    | Builtins.OpAdd -> Mir.OpCode.Add
                    | Builtins.OpSub -> Mir.OpCode.Sub
                    | Builtins.OpMul -> Mir.OpCode.Mul
                    | Builtins.OpDiv -> Mir.OpCode.Div
                    | Builtins.OpEq -> Mir.OpCode.Eq
                { ins = argIns @ [ Mir.Ins.TAC(dst, lhs, opcode, rhs) ]; res = Some(Mir.Value.RegVal dst) }
            | Hir.Callable.Fn sid ->
                match tid with
                | TypeId.Unit ->
                    { ins = argIns @ [ Mir.Ins.CallSym(sid, argValues) ]; res = None }
                | _ ->
                    let dst = declareTemp frame
                    { ins = argIns @ [ Mir.Ins.CallAssignSym(dst, sid, argValues) ]; res = Some(Mir.Value.RegVal dst) }
        | Hir.Expr.Block (stmts, expr, _, _) ->
            let stmtIns = stmts |> List.collect (layoutStmt frame)
            let exprKn = layoutExpr frame expr
            { ins = stmtIns @ exprKn.ins; res = exprKn.res }
        | Hir.Expr.If (cond, thenBranch, elseBranch, tid, _) ->
            let condKn = layoutExpr frame cond
            let thenKn = layoutExpr frame thenBranch
            let elseKn = layoutExpr frame elseBranch
            let thenLabel = Mir.Label()
            let elseLabel = Mir.Label()
            let endLabel = Mir.Label()

            match tid with
            | TypeId.Unit ->
                {
                    ins =
                        condKn.ins
                        @ [ Mir.Ins.JumpTrue(condKn.res.Value, thenLabel); Mir.Ins.JumpFalse(condKn.res.Value, elseLabel) ]
                        @ [ Mir.Ins.MarkLabel thenLabel ] @ thenKn.ins @ [ Mir.Ins.Jump endLabel ]
                        @ [ Mir.Ins.MarkLabel elseLabel ] @ elseKn.ins @ [ Mir.Ins.Jump endLabel ]
                        @ [ Mir.Ins.MarkLabel endLabel ]
                    res = None
                }
            | _ ->
                let dst = declareTemp frame
                {
                    ins =
                        condKn.ins
                        @ [ Mir.Ins.JumpTrue(condKn.res.Value, thenLabel); Mir.Ins.JumpFalse(condKn.res.Value, elseLabel) ]
                        @ [ Mir.Ins.MarkLabel thenLabel ]
                        @ thenKn.ins
                        @ [ Mir.Ins.Assign(dst, thenKn.res.Value); Mir.Ins.Jump endLabel ]
                        @ [ Mir.Ins.MarkLabel elseLabel ]
                        @ elseKn.ins
                        @ [ Mir.Ins.Assign(dst, elseKn.res.Value); Mir.Ins.Jump endLabel ]
                        @ [ Mir.Ins.MarkLabel endLabel ]
                    res = Some(Mir.Value.RegVal dst)
                }
        | Hir.Expr.ExprError (message, _, _) ->
            failwithf "Cannot lower erroneous expression: %s" message
        | Hir.Expr.Lambda _ ->
            failwith "Lambda lowering is not implemented"

    and private layoutStmt (frame: Mir.Frame) (stmt: Hir.Stmt) : Mir.Ins list =
        match stmt with
        | Hir.Stmt.Let (sid, _, value, _) ->
            let valueKn = layoutExpr frame value
            let dst = frame.addLoc sid
            match valueKn.res with
            | Some v -> valueKn.ins @ [ Mir.Ins.Assign(dst, v) ]
            | None -> valueKn.ins
        | Hir.Stmt.Assign (sid, value, _) ->
            let valueKn = layoutExpr frame value
            match frame.get sid, valueKn.res with
            | Some dst, Some v -> valueKn.ins @ [ Mir.Ins.Assign(dst, v) ]
            | Some _, None -> valueKn.ins
            | None, _ -> failwithf "Undefined variable in assignment: %A" sid
        | Hir.Stmt.ExprStmt (expr, _) ->
            (layoutExpr frame expr).ins
        | Hir.Stmt.ErrorStmt (message, _) ->
            failwithf "Cannot lower erroneous statement: %s" message

    let private layoutMethod (methodName: string) (hirMethod: Hir.Method) : Mir.Method =
        let frame = Mir.Frame()
        let bodyKn = layoutExpr frame hirMethod.body
        let body =
            match hirMethod.typ with
            | TypeId.Fn (_, TypeId.Unit) -> bodyKn.ins @ [ Mir.Ins.Ret ]
            | TypeId.Fn (_, _) ->
                match bodyKn.res with
                | Some v -> bodyKn.ins @ [ Mir.Ins.RetValue v ]
                | None -> bodyKn.ins @ [ Mir.Ins.Ret ]
            | _ -> failwithf "Expected function type for method: %A" hirMethod.typ

        match hirMethod.typ with
        | TypeId.Fn (args, ret) -> Mir.Method(methodName, hirMethod.sym, args, ret, body, frame)
        | _ -> failwithf "Expected function type for method: %A" hirMethod.typ

    let private layoutType (typeName: string) (hirType: Hir.Type) : Mir.Type =
        let fields = hirType.fields |> List.map (fun field -> Mir.Field(field.sym, field.typ))
        Mir.Type(typeName, hirType.sym, fields, [], [])

    let private layoutModule (hirModule: Hir.Module) : Mir.Module =
        let resolveTypeName (sid: SymbolId) =
            hirModule.scope.types
            |> Seq.tryPick (fun (KeyValue(name, tid)) ->
                match tid with
                | TypeId.Name tid when tid.id = sid.id -> Some name
                | _ -> None)
            |> Option.defaultValue (sprintf "type_%d" sid.id)

        let resolveMethodName (sid: SymbolId) =
            hirModule.scope.vars
            |> Seq.tryPick (fun (KeyValue(name, symSid)) ->
                if symSid.id = sid.id then Some name else None)
            |> Option.defaultValue (sprintf "fn_%d" sid.id)

        let types =
            hirModule.types
            |> List.map (fun hirType -> layoutType (resolveTypeName hirType.sym) hirType)

        let methods =
            hirModule.methods
            |> List.map (fun hirMethod -> layoutMethod (resolveMethodName hirMethod.sym) hirMethod)

        Mir.Module(hirModule.name, types, methods)

    let layoutAssembly (asmName: string, asm: Hir.Assembly) : Mir.Assembly =
        let modules = asm.modules |> List.map layoutModule
        Mir.Assembly(asmName, modules)
