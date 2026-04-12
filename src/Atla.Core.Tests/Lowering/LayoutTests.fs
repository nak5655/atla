namespace Atla.Core.Tests.Lowering

open Xunit
open Atla.Core.Syntax
open Atla.Core.Syntax.Data
open Atla.Core.Semantics
open Atla.Core.Semantics.Data
open Atla.Core.Lowering
open Atla.Core.Lowering.Data
open Atla.Core.Data

module LayoutTests =
    [<Fact>]
    let ``layoutAssembly emits RetValue for non-unit method`` () =
        let span = { left = Position.Zero; right = Position.Zero }
        let methodSym = SymbolId 0
        let scope = Scope(None)
        scope.DeclareVar("main", methodSym)

        let hirMethod =
            Hir.Method(
                methodSym,
                Hir.Expr.Int(42, span),
                TypeId.Fn([], TypeId.Int),
                span)

        let hirModule = Hir.Module("Main", [], [], [ hirMethod ], scope)
        let hirAssembly = Hir.Assembly("ignored", [ hirModule ])

        let mirAssembly = Layout.layoutAssembly("TestAsm", hirAssembly)
        let methodBody = mirAssembly.modules.Head.methods.Head.body

        Assert.Contains(
            Mir.Ins.RetValue(Mir.Value.ImmVal(Mir.Imm.Int 42)),
            methodBody)

    [<Fact>]
    let ``layoutAssembly lowers for statement into loop control instructions`` () =
        let program = """
import System.Console
import System.Linq.Enumerable

fn main: () =
    for i in (Enumerable.Range 1 3).GetEnumerator()
        Console.WriteLine i
"""

        let input: Input<SourceChar> = StringInput program
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Success (moduleAst, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", moduleAst) with
                | Result.Ok hirModule ->
                    let mirAssembly = Layout.layoutAssembly("TestAsm", Hir.Assembly("test", [ hirModule ]))
                    let methodBody = mirAssembly.modules.Head.methods.Head.body

                    let hasMoveNextCall =
                        methodBody
                        |> List.exists (function
                            | Mir.Ins.CallAssign (_, methodInfo, _) when methodInfo.Name = "MoveNext" -> true
                            | _ -> false)

                    let hasCurrentCall =
                        methodBody
                        |> List.exists (function
                            | Mir.Ins.CallAssign (_, methodInfo, _) when methodInfo.Name = "get_Current" -> true
                            | _ -> false)

                    let hasLoopControl =
                        methodBody
                        |> List.exists (function
                            | Mir.Ins.MarkLabel _
                            | Mir.Ins.Jump _
                            | Mir.Ins.JumpTrue _
                            | Mir.Ins.JumpFalse _ -> true
                            | _ -> false)

                    Assert.True(hasMoveNextCall, "MIRにMoveNext呼び出しがありません。")
                    Assert.True(hasCurrentCall, "MIRにCurrent取得呼び出しがありません。")
                    Assert.True(hasLoopControl, "MIRにループ制御命令（ラベル/ジャンプ）がありません。")
                | Result.Error diagnostics ->
                    let message =
                        diagnostics
                        |> List.map (fun err -> err.toString())
                        |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
