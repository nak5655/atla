namespace Atla.Compiler.Lowering

open System
open Atla.Compiler.Hir
open Atla.Compiler.Mir

module Layout =
    type KNormal(ins: Mir.Ins list, res: Mir.Value option) =
        member this.ins = ins
        member this.res = res

    let rec layoutExpr (frame: Frame) (expr: Hir.Expr) : KNormal =
        match expr with
        | :? Hir.Expr.Unit -> KNormal([], None)
        | :? Hir.Expr.Int as intExpr -> KNormal([], Some (Mir.Value.ImmVal(Mir.Imm.Int(intExpr.value))))
        | :? Hir.Expr.Float as floatExpr -> KNormal([], Some (Mir.Value.ImmVal(Mir.Imm.Float(floatExpr.value))))
        | :? Hir.Expr.String as stringExpr -> KNormal([], Some (Mir.Value.ImmVal(Mir.Imm.String(stringExpr.value))))
        | :? Hir.Expr.Id as idExpr ->
            let sysType = expr.typ.ToSystemType()
            match frame.resolve(Symbol(idExpr.name, sysType)) with
            | Some (FramePosition.Loc i) -> KNormal([], Some (Mir.Value.Loc i))
            | Some (FramePosition.Arg i) -> KNormal([], Some (Mir.Value.Arg i))
            | None -> failwithf "Undefined variable: %s" idExpr.name
        | :? Hir.Expr.Apply as applyExpr ->
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
                KNormal([], Some(Mir.Value.Field(fieldInfo)))
        | _ -> failwithf "Unsupported expression type: %A" (expr.GetType())

    and layoutStmt (frame: Frame) (stmt: Hir.Stmt) : Mir.Ins list =
        match stmt with
        | :? Hir.Stmt.Let as letStmt ->
            let valueK = layoutExpr frame letStmt.value
            let sym = Symbol(letStmt.name, letStmt.value.typ.ToSystemType())
            frame.declareLoc(sym)
            valueK.ins @ [Mir.Ins.Assign(sym, valueK.res.Value)]
        | :? Hir.Stmt.Assign as assignStmt ->
            let valueK = layoutExpr frame assignStmt.value
            let sym = Symbol(assignStmt.name, assignStmt.value.typ.ToSystemType())
            valueK.ins @ [Mir.Ins.Assign(sym, valueK.res.Value)]
        | :? Hir.Stmt.ExprStmt as exprStmt ->
            let exprK = layoutExpr frame exprStmt.expr
            exprK.ins
        | _ -> failwithf "Unsupported statement type: %A" (stmt.GetType())

    let layoutModule (hirModule: Hir.Module) : Mir.Module =
        let mutable methods = []
        let mutable types = []
        for decl in hirModule.decls do
            match decl with
            | Hir.Decl.Fn (name, args, ret, body, _) ->
                let frame = Frame()
                let bodyK = layoutExpr frame body
                let insts = match bodyK.res with
                            | Some res -> bodyK.ins @ [Mir.Ins.RetValue res]
                            | None -> bodyK.ins @ [Mir.Ins.Ret]
                let args = List.map (fun (arg: Symbol) -> arg.typ) frame.args
                methods <- Mir.Method(name, args, body.typ.ToSystemType(), insts, frame) :: methods
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