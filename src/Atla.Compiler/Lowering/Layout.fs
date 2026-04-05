namespace Atla.Compiler.Lowering

open System
open System.Collections.Generic
open Atla.Compiler.Semantics.Data
open Atla.Compiler.Lowering.Data

module Layout =
    type Env(symbolTable: SymbolTable, typeSubst: TypeSubst) =
        member this.symbolTable = symbolTable
        member this.typeSubst = typeSubst

        member this.resolveSym(sym: SymbolId) : SymbolInfo option =
            match symbolTable.Get(sym) with
            | Some info -> Some info
            | None -> None

        member this.resolveType(tid: TypeId): SymbolInfo option =
            match Type.resolve typeSubst tid with
            | TypeId.Name sym -> 
                match symbolTable.Get(sym) with
                | Some info -> Some info
                | None -> None
            | _ -> None

        member this.declareTemp (frame: Frame) (typ: TypeId): Mir.Reg =
            let sid = symbolTable.NextId()
            let name = sprintf "$temp%d" (frame.locs.Count)
            symbolTable.Add(sid, SymbolInfo(name, typ, SymbolKind.Local()))
            frame.addLoc(sid)

    type KNormal(ins: Mir.Ins list, res: Mir.Value option) =
        member this.ins = ins
        member this.res = res

    let rec layoutExpr (env: Env) (frame: Frame) (expr: Hir.Expr) : KNormal =
        match expr with
        | Hir.Expr.Unit (_) -> KNormal([], None)
        | Hir.Expr.Int (value, _) -> KNormal([], Some (Mir.Value.ImmVal(Mir.Imm.Int(value))))
        | Hir.Expr.Float (value, _) -> KNormal([], Some (Mir.Value.ImmVal(Mir.Imm.Float(value))))
        | Hir.Expr.String (value, _) -> KNormal([], Some (Mir.Value.ImmVal(Mir.Imm.String(value))))
        | Hir.Expr.Id (sym, _, _) ->
            match frame.get(sym) with
            | Some (Mir.Reg.Loc i) -> KNormal([], Some (Mir.Value.RegVal(Mir.Reg.Loc i)))
            | Some (Mir.Reg.Arg i) -> KNormal([], Some (Mir.Value.RegVal(Mir.Reg.Arg i)))
            | None -> failwithf "Undefined variable: %A" sym
        | Hir.Expr.Call (func, args, _, _) ->
            match func with
            | Hir.Expr.Id (sym, _, _) ->
                match env.resolveSym(sym) with
                | Some(symInfo) ->
                    match symInfo.kind with
                    | SymbolKind.NativeMethod mi ->
                        // 通常の関数呼び出し
                        let mutable ins = []
                        let mutable argValues = []
                        
                        for arg in args do
                            let argK = layoutExpr env frame arg
                            ins <- ins @ argK.ins
                            argValues <- argValues @ [argK.res.Value]

                        let funcK = layoutExpr env frame func
                        ins <- ins @ funcK.ins

                        match funcK.res with
                        | Some (Mir.Value.MethodVal methodInfo) ->
                            ins <- ins @ [Mir.Ins.Call(Choice1Of2(methodInfo), argValues)]
                            KNormal(ins, None)
                        | _ -> failwithf "Expected a method value in function application, but got: %A" funcK.res
                    | SymbolKind.BuiltinOperator op ->
                        //  組み込み関数呼び出し
                        let mutable ins = []
                        let mutable argValues = []

                        for arg in args do
                            let argK = layoutExpr env frame arg
                            ins <- ins @ argK.ins
                            argValues <- argValues @ [argK.res.Value]

                        let resReg = env.declareTemp frame expr.typ

                        match op with
                        | SymbolKind.OpAdd -> ins <- ins @ [Mir.Ins.TAC(resReg, argValues.[0], Mir.OpCode.Add, argValues.[1])]
                        | SymbolKind.OpSub -> ins <- ins @ [Mir.Ins.TAC(resReg, argValues.[0], Mir.OpCode.Sub, argValues.[1])]
                        | SymbolKind.OpMul -> ins <- ins @ [Mir.Ins.TAC(resReg, argValues.[0], Mir.OpCode.Mul, argValues.[1])]
                        | SymbolKind.OpDiv -> ins <- ins @ [Mir.Ins.TAC(resReg, argValues.[0], Mir.OpCode.Div, argValues.[1])]
                        | SymbolKind.OpEq -> ins <- ins @ [Mir.Ins.TAC(resReg, argValues.[0], Mir.OpCode.Eq, argValues.[1])]

                        KNormal(ins, Some(Mir.Value.RegVal(resReg)))
                | _ -> failwithf "Undefined symbol in function application: %A" sym
            | _ -> failwithf "Unsupported function expression in application: %A" func
        | Hir.Expr.Block (stmts, expr, _, _) ->
            let mutable ins = []
            for stmt in List.take (stmts.Length - 1) stmts do
                ins <- ins @ layoutStmt env frame stmt

            let exprK = layoutExpr env frame expr
            ins <- ins @ exprK.ins
            KNormal(ins, exprK.res)
        | Hir.Expr.MemberAccess (expr, name, typ, _) ->
            match env.resolveType(expr.typ) with
            | Some typeInfo ->
                let sysType = 
                    match typeInfo.kind with
                    | SymbolKind.SystemType t -> t.GetMethod(name, )
                    | _ -> failwithf "Expected a system type for member access, but got: %A" typeInfo.kind
                sysType
                let mi = sysType.GetMethod(name)
            
            match (memberAccess :> Hir.Expr).typ with
            | Type.Function (args, ret) ->
                let methodInfo = sysType.GetMethod(memberAccess.memberName, args |> List.map (fun t -> t.ToSystemType()) |> List.toArray)
                KNormal([], Some(Mir.Value.MethodVal(methodInfo)))
            | _ ->
                let fieldInfo = sysType.GetField(memberAccess.memberName)
                KNormal([], Some(Mir.Value.FieldVal(fieldInfo)))
        | :? Hir.Expr.If as ifExpr ->
            let mutable ins = []
            let thenLabel = Mir.Label()
            let elseLabel = Mir.Label()
            let endLabel = Mir.Label()
            let resReg = frame.addLoc((ifExpr :> Hir.Expr).typ.ToSystemType())

            // 条件式
            let condK = layoutExpr(symTbl, frame, ifExpr.cond)
            ins <- ins @ condK.ins
            ins <- ins @ [Mir.Ins.JumpTrue(condK.res.Value, thenLabel)]
            ins <- ins @ [Mir.Ins.JumpFalse(condK.res.Value, elseLabel)]

            // Then
            let thenK = layoutExpr(symTbl, frame, ifExpr.thenBranch)
            ins <- ins @ [Mir.Ins.MarkLabel(thenLabel)] @ thenK.ins
            ins <- ins @ [Mir.Ins.Assign(resReg, thenK.res.Value)]
            ins <- ins @ [Mir.Ins.Jump(endLabel)]

            // Else
            let elseK = layoutExpr(symTbl, frame, ifExpr.elseBranch)
            ins <- ins @ [Mir.Ins.MarkLabel(elseLabel)] @ elseK.ins
            ins <- ins @ [Mir.Ins.Assign(resReg, elseK.res.Value)]
            ins <- ins @ [Mir.Ins.Jump(endLabel)]

            ins <- ins @ [Mir.Ins.MarkLabel(endLabel)]
            KNormal(ins, Some (Mir.Value.RegVal resReg))
        | _ -> failwithf "Unsupported expression type: %A" (expr.GetType())

    and layoutStmt (env: Env) (frame: Frame) (stmt: Hir.Stmt) : Mir.Ins list =
        match stmt with
        | :? Hir.Stmt.Let as letStmt ->
            let valueK = layoutExpr(symTbl, frame, letStmt.value)
            let typ = letStmt.value.typ.ToSystemType()
            let reg = frame.addLoc(typ)
            symTbl.Add(letStmt.name, typ, reg)
            valueK.ins @ [Mir.Ins.Assign(reg, valueK.res.Value)]
        | :? Hir.Stmt.Assign as assignStmt ->
            let valueK = layoutExpr(symTbl, frame, assignStmt.value)
            let typ = assignStmt.value.typ.ToSystemType()
            match symTbl.resolve(assignStmt.name, typ) with
            | Some reg -> valueK.ins @ [Mir.Ins.Assign(reg, valueK.res.Value)]
            | None -> failwithf "Undefined variable: %s" assignStmt.name
        | :? Hir.Stmt.ExprStmt as exprStmt ->
            let exprK = layoutExpr(symTbl, frame, exprStmt.expr)
            exprK.ins
        | _ -> failwithf "Unsupported statement type: %A" (stmt.GetType())

    let layoutType (hirType: Hir.Type) : Mir.Type =
        let mutable fields = []
        let mutable ctors = []
        let mutable methods = []

        for field in hirType.fields do
            fields <- Mir.Field(field.name, field.typ.ToSystemType()) :: fields
        for ctor in hirType.ctors do
            let symTable = SymbolTable()
            let frame = Mir.Frame()
            let ins = layoutExpr(symTable, frame, ctor.body) |> fun k -> k.ins @ [Mir.Ins.Ret]
            ctors <- Mir.Constructor(ctor.args |> List.map (fun t -> t.typ.ToSystemType()), ins, frame) :: ctors
        for method in hirType.methods do
            let symTable = SymbolTable()
            let frame = Mir.Frame()
            let ins = layoutExpr(symTable, frame, method.body) |> fun k -> k.ins @ [Mir.Ins.Ret]
            methods <- Mir.Method(method.name, method.args |> List.map (fun t -> t.typ.ToSystemType()), method.ret.ToSystemType(), ins, frame) :: methods

        Mir.Type(hirType.name, List.rev fields, List.rev ctors, List.rev methods)

    let layoutModule (env: Env) (hirModule: Hir.Module) : Mir.Module =
        let mutable types = []
        let mutable methods = []

        for typ in hirModule.types do
            types <- layoutType typ :: types
        for method in hirModule.methods do
            let symTable = SymbolTable()
            let frame = Mir.Frame()
            let ins = layoutExpr(symTable, frame, method.body) |> fun k -> k.ins @ [Mir.Ins.Ret]
            methods <- Mir.Method(method.name, method.args |> List.map (fun t -> t.typ.ToSystemType()), method.ret.ToSystemType(), ins, frame) :: methods

        Mir.Module(hirModule.name, types, methods)

    let layoutAssembly (env: Env) (asm: Hir.Assembly) : Mir.Assembly =
        let mutable mirModules = []
        for mdul in asm.modules do
            mirModules <- layoutModule env mdul :: mirModules
        Mir.Assembly(asm.name, List.rev mirModules)