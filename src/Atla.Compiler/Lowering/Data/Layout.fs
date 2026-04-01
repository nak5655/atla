namespace Atla.Compiler.Lowering.Data

open System
open System.Collections.Generic

module Layout =
    type KNormal(ins: Mir.Ins list, res: Mir.Value option) =
        member this.ins = ins
        member this.res = res

    let rec layoutExpr (symTbl: SymbolTable, frame: Mir.Frame, expr: Hir.Expr) : KNormal =
        match expr with
        | :? Hir.Expr.Unit -> KNormal([], None)
        | :? Hir.Expr.Int as intExpr -> KNormal([], Some (Mir.Value.ImmVal(Mir.Imm.Int(intExpr.value))))
        | :? Hir.Expr.Float as floatExpr -> KNormal([], Some (Mir.Value.ImmVal(Mir.Imm.Float(floatExpr.value))))
        | :? Hir.Expr.String as stringExpr -> KNormal([], Some (Mir.Value.ImmVal(Mir.Imm.String(stringExpr.value))))
        | :? Hir.Expr.Id as idExpr ->
            let sysType = expr.typ.ToSystemType()
            match symTbl.resolve(idExpr.name, sysType) with
            | Some (Mir.Reg.Loc i) -> KNormal([], Some (Mir.Value.RegVal(Mir.Reg.Loc i)))
            | Some (Mir.Reg.Arg i) -> KNormal([], Some (Mir.Value.RegVal(Mir.Reg.Arg i)))
            | None -> failwithf "Undefined variable: %s" idExpr.name
        | :? Hir.Expr.Apply as applyExpr ->
            match applyExpr.func with
            | :? Hir.Expr.Id as idExpr ->
                match idExpr.symbol.info with
                | SymbolInfo.Method mi ->
                    // 通常の関数呼び出し
                    let mutable ins = []
                    let mutable argValues = []
                    for arg in applyExpr.args do
                        let argK = layoutExpr(symTbl, frame, arg)
                        ins <- ins @ argK.ins
                        argValues <- argValues @ [argK.res.Value]

                    let funcK = layoutExpr(symTbl, frame, applyExpr.func)
                    ins <- ins @ funcK.ins
                    match funcK.res with
                    | Some (Mir.Value.MethodVal methodInfo) ->
                        ins <- ins @ [Mir.Ins.Call(Choice1Of2(methodInfo), argValues)]
                        KNormal(ins, None)
                    | _ -> failwithf "Expected a method value in function application, but got: %A" funcK.res
                | SymbolInfo.NativeMethod impl ->
                    // ネイティブ関数呼び出し
                    let mutable ins = []
                    let mutable argValues = []
                    for arg in applyExpr.args do
                        let argK = layoutExpr(symTbl, frame, arg)
                        ins <- ins @ argK.ins
                        argValues <- argValues @ [argK.res.Value]
                    let temp = frame.addLoc()
                    ins <- ins @ [ice2Of2 impl, argValues)]
                    KNormal(ins, None)
            | _ -> failwithf "Unsupported function expression in application: %A" applyExpr.func
        | :? Hir.Expr.Block as blockExpr ->
            let mutable ins = []
            for stmt in List.take (blockExpr.stmts.Length - 1) blockExpr.stmts do
                ins <- ins @ layoutStmt(symTbl, frame, stmt)

            // The last statement can be an expression statement, which determines the block's value
            match List.last blockExpr.stmts with
            | :? Hir.Stmt.ExprStmt as exprStmt ->
                let sysType = exprStmt.expr.typ.ToSystemType()
                if sysType = typeof<Void> then
                    ins <- ins @ layoutStmt(symTbl, frame, exprStmt)
                    KNormal(ins, None)
                else
                    let exprK = layoutExpr(symTbl, frame, exprStmt.expr)
                    ins <- ins @ exprK.ins
                    KNormal(ins, exprK.res)
            | stmt ->
                ins <- ins @ layoutStmt(symTbl, frame, stmt)
                ins <- ins @ [Mir.Ins.Ret]
                KNormal(ins, None)
        | :? Hir.Expr.MemberAccess as memberAccess ->
            let sysType = memberAccess.receiver.typ.ToSystemType()
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

    and layoutStmt (symTbl: SymbolTable, frame: Mir.Frame, stmt: Hir.Stmt) : Mir.Ins list =
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

    let layoutModule (hirModule: Hir.Module) : Mir.Module =
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

    let layoutAssembly (asm: Hir.Assembly) : Mir.Assembly =
        let mutable mirModules = []
        for mdul in asm.modules do
            mirModules <- layoutModule mdul :: mirModules
        Mir.Assembly(asm.name, List.rev mirModules)