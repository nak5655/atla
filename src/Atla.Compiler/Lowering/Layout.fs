namespace Atla.Compiler.Lowering

open System
open System.Collections.Generic
open Atla.Compiler.Hir
open Atla.Compiler.Mir

module Layout =
    // フレームレイアウトを決定するために識別子を
    type LabeledFrame() =
        let _frame: Mir.Frame = Mir.Frame()
        let labels: Dictionary<string, Mir.Reg> = Dictionary()

        member this.frame = _frame

        member this.declareArg(id: string, typ: System.Type): Mir.Reg =
            let reg = _frame.addArg(typ)
            labels.Add(id, reg)
            reg

        member this.declareLoc(id: string, typ: System.Type): Mir.Reg =
            let reg = _frame.addLoc(typ)
            labels.Add(id, reg)
            reg

        member this.declareTemp(typ: System.Type): Mir.Reg =
            _frame.addLoc(typ)

        member this.resolve(id: string): Mir.Reg option =
            if labels.ContainsKey(id) then Some labels.[id] else None

    type KNormal(ins: Mir.Ins list, res: Mir.Value option) =
        member this.ins = ins
        member this.res = res
        
    let tryGetBinOp (name: string) : Mir.OpCode option =
        match name with
        | "+" -> Some Mir.OpCode.Add
        | "-" -> Some Mir.OpCode.Sub
        | "*" -> Some Mir.OpCode.Mul
        | "/" -> Some Mir.OpCode.Div
        | "%" -> Some Mir.OpCode.Mod
        | "||" -> Some Mir.OpCode.Or
        | "&&" -> Some Mir.OpCode.And
        | "==" -> Some Mir.OpCode.Eq
        | _ -> None

    let rec layoutExpr (frame: LabeledFrame) (expr: Hir.Expr) : KNormal =
        match expr with
        | :? Hir.Expr.Unit -> KNormal([], None)
        | :? Hir.Expr.Int as intExpr -> KNormal([], Some (Mir.Value.ImmVal(Mir.Imm.Int(intExpr.value))))
        | :? Hir.Expr.Float as floatExpr -> KNormal([], Some (Mir.Value.ImmVal(Mir.Imm.Float(floatExpr.value))))
        | :? Hir.Expr.String as stringExpr -> KNormal([], Some (Mir.Value.ImmVal(Mir.Imm.String(stringExpr.value))))
        | :? Hir.Expr.Id as idExpr ->
            let sysType = expr.typ.ToSystemType()
            match frame.resolve(idExpr.name) with
            | Some (Mir.Reg.Loc i) -> KNormal([], Some (Mir.Value.RegVal(Mir.Reg.Loc i)))
            | Some (Mir.Reg.Arg i) -> KNormal([], Some (Mir.Value.RegVal(Mir.Reg.Arg i)))
            | None -> failwithf "Undefined variable: %s" idExpr.name
        | :? Hir.Expr.Apply as applyExpr ->
            // 二項演算子の場合は TAC命令に変換する
            let binOp =
                match applyExpr.func with
                | :? Hir.Expr.Id as idExpr when applyExpr.args.Length = 2 -> tryGetBinOp idExpr.name
                | _ -> None

            match binOp with
            | Some op ->
                // 二項演算子の適用
                let lhsK = layoutExpr frame applyExpr.args.[0]
                let rhsK = layoutExpr frame applyExpr.args.[1]
                let destSym = frame.declareTemp((applyExpr :> Hir.Expr).typ.ToSystemType())
                let ins = lhsK.ins @ rhsK.ins @ [Mir.Ins.TAC(destSym, lhsK.res.Value, op, rhsK.res.Value)]
                KNormal(ins, Some (Mir.Value.RegVal destSym))
            | None ->
                // 通常の関数呼び出し
                let mutable ins = []
                let mutable argValues = []
                for arg in applyExpr.args do
                    let argK = layoutExpr frame arg
                    ins <- ins @ argK.ins
                    argValues <- argValues @ [argK.res.Value]

                let funcK = layoutExpr frame applyExpr.func
                ins <- ins @ funcK.ins
                match funcK.res with
                | Some (Mir.Value.MethodVal methodInfo) ->
                    ins <- ins @ [Mir.Ins.Call(Choice1Of2(methodInfo), argValues)]
                    KNormal(ins, None)
                | _ -> failwithf "Expected a method value in function application, but got: %A" funcK.res
        | :? Hir.Expr.Block as blockExpr ->
            let mutable ins = []
            for stmt in List.take (blockExpr.stmts.Length - 1) blockExpr.stmts do
                ins <- ins @ layoutStmt frame stmt

            // The last statement can be an expression statement, which determines the block's value
            match List.last blockExpr.stmts with
            | :? Hir.Stmt.ExprStmt as exprStmt ->
                let sysType = exprStmt.expr.typ.ToSystemType()
                if sysType = typeof<Void> then
                    ins <- ins @ layoutStmt frame exprStmt
                    KNormal(ins, None)
                else
                    let exprK = layoutExpr frame exprStmt.expr
                    ins <- ins @ exprK.ins
                    KNormal(ins, exprK.res)
            | stmt ->
                ins <- ins @ layoutStmt frame stmt
                ins <- ins @ [Mir.Ins.Ret]
                KNormal(ins, None)
        | :? Hir.Expr.MemberAccess as memberAccess ->
            let sysType = memberAccess.receiver.typ.ToSystemType()
            match (memberAccess :> Hir.Expr).typ with
            | TypeCray.Function (args, ret) ->
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
            let resSym = frame.declareTemp((ifExpr :> Hir.Expr).typ.ToSystemType())

            // 条件式
            let condK = layoutExpr frame ifExpr.cond
            ins <- ins @ condK.ins
            ins <- ins @ [Mir.Ins.JumpTrue(condK.res.Value, thenLabel)]
            ins <- ins @ [Mir.Ins.JumpFalse(condK.res.Value, elseLabel)]

            // Then
            let thenK = layoutExpr frame ifExpr.thenBranch
            ins <- ins @ [Mir.Ins.MarkLabel(thenLabel)] @ thenK.ins
            ins <- ins @ [Mir.Ins.Assign(resSym, thenK.res.Value)]
            ins <- ins @ [Mir.Ins.Jump(endLabel)]

            // Else
            let elseK = layoutExpr frame ifExpr.elseBranch
            ins <- ins @ [Mir.Ins.MarkLabel(elseLabel)] @ elseK.ins
            ins <- ins @ [Mir.Ins.Assign(resSym, elseK.res.Value)]
            ins <- ins @ [Mir.Ins.Jump(endLabel)]

            ins <- ins @ [Mir.Ins.MarkLabel(endLabel)]
            KNormal(ins, Some (Mir.Value.RegVal resSym))
        | _ -> failwithf "Unsupported expression type: %A" (expr.GetType())

    and layoutStmt (frame: LabeledFrame) (stmt: Hir.Stmt) : Mir.Ins list =
        match stmt with
        | :? Hir.Stmt.Let as letStmt ->
            let valueK = layoutExpr frame letStmt.value
            let dest = frame.declareLoc(letStmt.name, letStmt.value.typ.ToSystemType())
            valueK.ins @ [Mir.Ins.Assign(dest, valueK.res.Value)]
        | :? Hir.Stmt.Assign as assignStmt ->
            let valueK = layoutExpr frame assignStmt.value
            match frame.resolve(assignStmt.name) with
            | Some reg ->
                valueK.ins @ [Mir.Ins.Assign(reg, valueK.res.Value)]
            | None -> failwithf "Undefined variable: %s" assignStmt.name
        | :? Hir.Stmt.ExprStmt as exprStmt ->
            let exprK = layoutExpr frame exprStmt.expr
            exprK.ins
        | _ -> failwithf "Unsupported statement type: %A" (stmt.GetType())

    let layoutModule (hirModule: Hir.Module) : Mir.Module =
        let mutable methods = []
        let mutable types = []
        for decl in hirModule.decls do
            match decl with
            | Hir.Decl.Fn (name, args, ret, body, scope, _) ->
                let frame = LabeledFrame()
                for arg in args do
                    match arg with
                    | :? Hir.FnArg.Unit -> ()
                    | :? Hir.FnArg.Named as namedArg ->
                        frame.declareArg(namedArg.name, namedArg.typ.ToSystemType()) |> ignore
                    | _ -> failwithf "Unsupported function argument type: %A" (arg.GetType())
                let bodyK = layoutExpr frame body
                let insts = match bodyK.res with
                            | Some res -> bodyK.ins @ [Mir.Ins.RetValue res]
                            | None -> bodyK.ins @ [Mir.Ins.Ret]
                methods <- Mir.Method(name, Seq.toList frame.frame.args, body.typ.ToSystemType(), insts, frame.frame) :: methods
            | Hir.Decl.TypeDef (name, typeExpr, _) ->
                // TODO
                types <- Mir.Type(name, [], [], []) :: types
            | Hir.Decl.DeclError (message, span) ->
                failwithf "Error in declaration: %s at %A" message span
        Mir.Module(hirModule.name, types, methods)

    let layoutAssembly (name: string, hirAssembly: Hir.Assembly) : Mir.Assembly =
        let mutable mirModules = []
        for mdul in hirAssembly.modules do
            mirModules <- layoutModule mdul :: mirModules
        Mir.Assembly(name, List.rev mirModules)