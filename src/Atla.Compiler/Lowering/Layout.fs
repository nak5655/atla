namespace Atla.Compiler.Lowering

open System
open System.Collections.Generic
open Atla.Compiler.Semantics.Data
open Atla.Compiler.Lowering.Data

module Layout =
    type Env(symbolTable: SymbolTable) =

        member this.resolveSym(sym: SymbolId) : SymbolInfo option =
            match symbolTable.Get(sym) with
            | Some info -> Some info
            | None -> None

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
        | Hir.Expr.Call (func, instance, args, _, _) ->
            match func with
            | Hir.Callable.NativeMethod mi ->
                // 通常の関数呼び出し
                let mutable ins = []
                let mutable argValues = []
                
                for arg in args do
                    let argK = layoutExpr env frame arg
                    ins <- ins @ argK.ins
                    argValues <- argValues @ [argK.res.Value]
                let resReg = env.declareTemp frame expr.typ
                ins <- ins @ [Mir.Ins.Call(Choice1Of2(mi), argValues)]
                KNormal(ins, Some(Mir.Value.RegVal(resReg))) 
            | Hir.Callable.BuiltinOperator op ->
                // 組み込み演算子呼び出し
                let mutable ins = []
                let mutable argValues = []
                for arg in args do
                    let argK = layoutExpr env frame arg
                    ins <- ins @ argK.ins
                    argValues <- argValues @ [argK.res.Value]
                let resReg = env.declareTemp frame expr.typ
                match op with
                | Builtins.OpAdd -> ins <- ins @ [Mir.Ins.TAC(resReg, argValues.[0], Mir.OpCode.Add, argValues.[1])]
                | Builtins.OpSub -> ins <- ins @ [Mir.Ins.TAC(resReg, argValues.[0], Mir.OpCode.Sub, argValues.[1])]
                | Builtins.OpMul -> ins <- ins @ [Mir.Ins.TAC(resReg, argValues.[0], Mir.OpCode.Mul, argValues.[1])]
                | Builtins.OpDiv -> ins <- ins @ [Mir.Ins.TAC(resReg, argValues.[0], Mir.OpCode.Div, argValues.[1])]
                | Builtins.OpEq -> ins <- ins @ [Mir.Ins.TAC(resReg, argValues.[0], Mir.OpCode.Eq, argValues.[1])]
                KNormal(ins, Some(Mir.Value.RegVal(resReg)))
            | Hir.Callable.Fn sym ->
                // TODO: 関数呼び出しの解決
                failwithf "Function calls by symbol are not yet implemented: %A" sym
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
        | Hir.Expr.If (cond, thenBranch, elseBranch, typ, _) ->
            let mutable ins = []
            let thenLabel = Mir.Label()
            let elseLabel = Mir.Label()
            let endLabel = Mir.Label()
            let resReg = env.declareTemp frame typ

            // 条件式
            let condK = layoutExpr env frame cond
            ins <- ins @ condK.ins
            ins <- ins @ [Mir.Ins.JumpTrue(condK.res.Value, thenLabel)]
            ins <- ins @ [Mir.Ins.JumpFalse(condK.res.Value, elseLabel)]

            // Then
            let thenK = layoutExpr env frame thenBranch
            ins <- ins @ [Mir.Ins.MarkLabel(thenLabel)] @ thenK.ins
            ins <- ins @ [Mir.Ins.Assign(resReg, thenK.res.Value)]
            ins <- ins @ [Mir.Ins.Jump(endLabel)]

            // Else
            let elseK = layoutExpr env frame elseBranch
            ins <- ins @ [Mir.Ins.MarkLabel(elseLabel)] @ elseK.ins
            ins <- ins @ [Mir.Ins.Assign(resReg, elseK.res.Value)]
            ins <- ins @ [Mir.Ins.Jump(endLabel)]

            ins <- ins @ [Mir.Ins.MarkLabel(endLabel)]
            KNormal(ins, Some (Mir.Value.RegVal resReg))
        | _ -> failwithf "Unsupported expression type: %A" (expr.GetType())

    and layoutStmt (env: Env) (frame: Frame) (stmt: Hir.Stmt) : Mir.Ins list =
        match stmt with
        | Hir.Stmt.Let (sym, typ, body, _) ->
            let bodyK = layoutExpr env frame body
            let reg = frame.addLoc sym
            bodyK.ins @ [Mir.Ins.Assign(reg, bodyK.res.Value)]
        | Hir.Stmt.Assign (sym, body, _) ->
            let bodyK = layoutExpr env frame body
            match frame.get sym with
            | Some (Mir.Reg.Loc i) -> bodyK.ins @ [Mir.Ins.Assign(Mir.Reg.Loc i, bodyK.res.Value)]
            | Some (Mir.Reg.Arg i) -> bodyK.ins @ [Mir.Ins.Assign(Mir.Reg.Arg i, bodyK.res.Value)]
            | None -> failwithf "Undefined variable in assignment: %A" sym
        | Hir.Stmt.ExprStmt (expr, _) ->
            let exprK = layoutExpr env frame expr
            exprK.ins
        | _ -> failwithf "Unsupported statement type: %A" (stmt.GetType())

    let layoutMethod (env: Env) (hirMethod: Hir.Method) : Mir.Method =
        let bodyK = layoutExpr env (Frame()) hirMethod.body
        match hirMethod.typ with
        | TypeId.Fn (args, ret) -> Mir.Method(hirMethod.sym, args, ret, bodyK.ins)
        | _ -> failwithf "Expected a function type for method, but got: %A" hirMethod.typ

    let layoutType (hirType: Hir.Type) : Mir.Type =
        let fields = List<Mir.Field>()
        let ctors = List<Mir.Constructor>()
        let methods = List<Mir.Method>()

        for field in hirType.fields do
            fields.Add(Mir.Field(field.sym, field.typ))
        // TODO constructors and methods

        Mir.Type(hirType.sym, Seq.toList fields, Seq.toList ctors, Seq.toList methods)

    let layoutModule (env: Env) (hirModule: Hir.Module) : Mir.Module =
        let types = List<Mir.Type>()
        let methods = List<Mir.Method>()

        for typ in hirModule.types do
            types.Add(layoutType typ)

        for method in hirModule.methods do
            match env.resolveSym(method.sym) with
            | Some symInfo ->
                match symInfo.typ with
                | TypeId.Fn (args, ret) ->
                    let methodInfo = symInfo.kind |> function
                        | SymbolKind.NativeMethod miList -> miList |> List.find (fun mi -> mi.GetParameters().Length = args.Length && mi.ReturnType = ret.ToSystemType())
                        | _ -> failwithf "Expected a native method for module-level method, but got: %A" symInfo.kind
                    methods.Add(Mir.Method(method.sym, args, ret, ))

        Mir.Module(hirModule.name, Seq.toList types, Seq.toList methods)

    let layoutAssembly (env: Env) (asm: Hir.Assembly) : Mir.Assembly =
        let mutable mirModules = []
        for mdul in asm.modules do
            mirModules <- layoutModule env mdul :: mirModules
        Mir.Assembly(asm.name, List.rev mirModules)