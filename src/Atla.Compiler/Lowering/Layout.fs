namespace Atla.Compiler.Lowering

open Atla.Compiler.Types
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
            KNormal(ins, funcK.res)
        | _ -> failwithf "Unsupported expression type: %A" (expr.GetType())

    let layoutModule (hirModule: Hir.Module) : Mir.Module =
        let mutable methods = []
        let mutable types = []
        for decl in hirModule.decls do
            match decl with
            | Hir.Decl.Def (name, expr, _) ->
                let frame = Frame()
                let rhsK = layoutExpr frame expr
                let insts = match rhsK.res with
                            | Some res -> rhsK.ins @ [Mir.Ins.RetValue res]
                            | None -> rhsK.ins @ [Mir.Ins.Ret]
                let args = List.map (fun (arg: Symbol) -> Mir.Argum(arg.name, arg.typ)) frame.args
                methods <- Mir.Method(name, args, expr.typ.ToSystemType(), insts, frame) :: methods
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