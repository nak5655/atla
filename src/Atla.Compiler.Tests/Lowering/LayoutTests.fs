namespace Atla.Compiler.Tests.Lowering

open Xunit
open Atla.Compiler.Semantics.Data
open Atla.Compiler.Lowering
open Atla.Compiler.Lowering.Data
open Atla.Compiler.Data

module LayoutTests =
    [<Fact>]
    let ``layoutAssembly emits RetValue for non-unit method`` () =
        let span = { left = Position.Zero; right = Position.Zero }
        let methodSym = SymbolId 0
        let hirMethod =
            Hir.Method(
                methodSym,
                Hir.Expr.Int(42, span),
                TypeId.Fn([], TypeId.Int),
                span)

        let hirModule = Hir.Module("Main", [], [], [ hirMethod ], Scope.GlobalScope())
        let hirAssembly = Hir.Assembly("ignored", [ hirModule ])

        let mirAssembly = Layout.layoutAssembly("TestAsm", hirAssembly)
        let methodBody = mirAssembly.modules.Head.methods.Head.body

        Assert.Contains(
            Mir.Ins.RetValue(Mir.Value.ImmVal(Mir.Imm.Int 42)),
            methodBody)
