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
                [],
                Hir.Expr.Int(42, span),
                TypeId.Fn([], TypeId.Int),
                span)

        let hirModule = Hir.Module("Main", [], [], [ hirMethod ], scope)
        let hirAssembly = Hir.Assembly("ignored", [ hirModule ])

        let mirAssemblyResult = Layout.layoutAssembly("TestAsm", hirAssembly)
        let mirAssembly =
            match mirAssemblyResult with
            | { succeeded = true; value = Some asm } -> asm
            | { diagnostics = diagnostics } ->
                let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                failwith $"layoutAssembly failed: {message}"
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
                | { succeeded = true; value = Some hirModule } ->
                    let mirAssemblyResult = Layout.layoutAssembly("TestAsm", Hir.Assembly("test", [ hirModule ]))
                    let mirAssembly =
                        match mirAssemblyResult with
                        | { succeeded = true; value = Some asm } -> asm
                        | { diagnostics = diagnostics } ->
                            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                            failwith $"layoutAssembly failed: {message}"
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
                | { diagnostics = diagnostics } ->
                    let message =
                        diagnostics
                        |> List.map (fun err -> err.toDisplayText())
                        |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``layoutAssembly preserves Array String argument type in MIR method signature`` () =
        let program = """
fn keep (xs: Array String): Array String = xs
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
                | { succeeded = true; value = Some hirModule } ->
                    let mirAssemblyResult = Layout.layoutAssembly("TestAsm", Hir.Assembly("test", [ hirModule ]))
                    let mirAssembly =
                        match mirAssemblyResult with
                        | { succeeded = true; value = Some asm } -> asm
                        | { diagnostics = diagnostics } ->
                            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                            failwith $"layoutAssembly failed: {message}"

                    let keepMethod =
                        mirAssembly.modules.Head.methods
                        |> List.tryFind (fun mirMethod -> mirMethod.name = "keep")

                    match keepMethod with
                    | Some mirMethod ->
                        Assert.Equal<TypeId list>([ TypeId.App(TypeId.Native typeof<System.Array>, [ TypeId.String ]) ], mirMethod.args)
                        Assert.Equal(TypeId.App(TypeId.Native typeof<System.Array>, [ TypeId.String ]), mirMethod.ret)
                    | None ->
                        Assert.True(false, "MIR method 'keep' was not found.")
                | { diagnostics = diagnostics } ->
                    let message =
                        diagnostics
                        |> List.map (fun err -> err.toDisplayText())
                        |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``Array String type snapshot stays stable across AST HIR MIR`` () =
        let program = "fn keep (xs: Array String): Array String = xs"
        let input: Input<SourceChar> = StringInput program

        let rec snapshotTypeExpr (typeExpr: Ast.TypeExpr) : string =
            match typeExpr with
            | :? Ast.TypeExpr.Id as idType -> $"Id({idType.name})"
            | :? Ast.TypeExpr.Unit -> "Unit"
            | :? Ast.TypeExpr.Apply as applyType ->
                let argSnapshot =
                    applyType.args
                    |> List.map snapshotTypeExpr
                    |> String.concat ","
                $"Apply({snapshotTypeExpr applyType.head},[{argSnapshot}])"
            | _ -> "Unsupported"

        let rec snapshotTypeId (tid: TypeId) : string =
            match tid with
            | TypeId.String -> "String"
            | TypeId.Int -> "Int"
            | TypeId.Unit -> "Unit"
            | TypeId.Native t when t = typeof<System.Array> -> "ArrayCtor"
            | TypeId.App (head, args) ->
                let argSnapshot = args |> List.map snapshotTypeId |> String.concat ","
                $"App({snapshotTypeId head},[{argSnapshot}])"
            | TypeId.Fn (args, ret) ->
                let argSnapshot = args |> List.map snapshotTypeId |> String.concat ","
                $"Fn([{argSnapshot}],{snapshotTypeId ret})"
            | TypeId.Error message -> $"Error({message})"
            | other -> sprintf "%A" other

        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Success (moduleAst, _) ->
                let astSnapshot =
                    match moduleAst.decls with
                    | [ (:? Ast.Decl.Fn as fnDecl) ] ->
                        match fnDecl.args with
                        | [ (:? Ast.FnArg.Named as namedArg) ] ->
                            $"arg={snapshotTypeExpr namedArg.typeExpr};ret={snapshotTypeExpr fnDecl.ret}"
                        | _ -> "unexpected-args"
                    | _ -> "unexpected-decls"

                Assert.Equal("arg=Apply(Id(Array),[Id(String)]);ret=Apply(Id(Array),[Id(String)])", astSnapshot)

                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", moduleAst) with
                | { succeeded = true; value = Some hirModule } ->
                    let hirSnapshot =
                        hirModule.methods
                        |> List.tryFind (fun methodInfo -> methodInfo.sym.id = (hirModule.scope.vars.["keep"]).id)
                        |> Option.map (fun methodInfo -> snapshotTypeId (Type.resolve subst methodInfo.typ))
                        |> Option.defaultValue "missing"

                    Assert.Equal("Fn([App(ArrayCtor,[String])],App(ArrayCtor,[String]))", hirSnapshot)

                    let mirAssemblyResult = Layout.layoutAssembly("TestAsm", Hir.Assembly("test", [ hirModule ]))
                    let mirSnapshot =
                        match mirAssemblyResult with
                        | { succeeded = true; value = Some asm } ->
                            asm.modules.Head.methods
                            |> List.tryFind (fun mirMethod -> mirMethod.name = "keep")
                            |> Option.map (fun mirMethod ->
                                let argsSnapshot = mirMethod.args |> List.map snapshotTypeId |> String.concat ","
                                $"args=[{argsSnapshot}];ret={snapshotTypeId mirMethod.ret}")
                            |> Option.defaultValue "missing"
                        | { diagnostics = diagnostics } ->
                            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                            failwith $"layoutAssembly failed: {message}"

                    Assert.Equal("args=[App(ArrayCtor,[String])];ret=App(ArrayCtor,[String])", mirSnapshot)
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``layoutAssembly returns explicit diagnostic for captured lambda before frame allocation`` () =
        let span = { left = Position.Zero; right = Position.Zero }
        let methodSym = SymbolId 100
        let capturedSym = SymbolId 101
        let scope = Scope(None)
        scope.DeclareVar("main", methodSym)
        scope.DeclareVar("captured", capturedSym)

        // 自由変数 `captured` を参照するラムダを構築する。
        let lambdaExpr =
            Hir.Expr.Lambda(
                [ Hir.Arg(SymbolId 102, "x", TypeId.Int, span) ],
                TypeId.Int,
                Hir.Expr.Id(capturedSym, TypeId.Int, span),
                TypeId.Fn([ TypeId.Int ], TypeId.Int),
                span)

        // メソッド本体はラムダを返す式にする（Layout 前処理で検出されることを期待）。
        let hirMethod =
            Hir.Method(
                methodSym,
                [],
                lambdaExpr,
                TypeId.Fn([], TypeId.Fn([ TypeId.Int ], TypeId.Int)),
                span)

        let hirModule = Hir.Module("Main", [], [], [ hirMethod ], scope)
        let hirAssembly = Hir.Assembly("test", [ hirModule ])
        let result = Layout.layoutAssembly("TestAsm", hirAssembly)

        Assert.False(result.succeeded, "captured lambda should fail in preprocessing stage")
        let message = result.diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
        Assert.Contains("Closure conversion requires env-class lowering", message)
        Assert.Contains("methodSid=100", message)
        Assert.Contains("captured=[101]", message)

    [<Fact>]
    let ``layoutAssembly lowers non-captured lambda via closure conversion`` () =
        let span = { left = Position.Zero; right = Position.Zero }
        let methodSym = SymbolId 200
        let lambdaArgSid = SymbolId 201
        let scope = Scope(None)
        scope.DeclareVar("main", methodSym)

        // 非捕捉ラムダ（引数をそのまま返す）をメソッド戻り値に置く。
        let lambdaExpr =
            Hir.Expr.Lambda(
                [ Hir.Arg(lambdaArgSid, "x", TypeId.Int, span) ],
                TypeId.Int,
                Hir.Expr.Id(lambdaArgSid, TypeId.Int, span),
                TypeId.Fn([ TypeId.Int ], TypeId.Int),
                span)

        let hirMethod =
            Hir.Method(
                methodSym,
                [],
                lambdaExpr,
                TypeId.Fn([], TypeId.Fn([ TypeId.Int ], TypeId.Int)),
                span)

        let hirModule = Hir.Module("Main", [], [], [ hirMethod ], scope)
        let hirAssembly = Hir.Assembly("test", [ hirModule ])
        let result = Layout.layoutAssembly("TestAsm", hirAssembly)

        Assert.True(result.succeeded, "non-captured lambda should be lowered by closure conversion")
        match result.value with
        | Some mirAsm ->
            let moduleMethods = mirAsm.modules |> List.head |> fun modul -> modul.methods
            Assert.True(moduleMethods.Length >= 2, "lifted lambda method should be appended to module methods")
            let mainMethod = moduleMethods |> List.find (fun methodInfo -> methodInfo.name = "main")
            let hasDelegateReturn =
                mainMethod.body
                |> List.exists (function
                    | Mir.Ins.RetValue(Mir.Value.FnDelegate _) -> true
                    | _ -> false)
            Assert.True(hasDelegateReturn, "main should return FnDelegate of lifted method")
        | None ->
            Assert.True(false, "layoutAssembly succeeded but returned no MIR assembly")

    [<Fact>]
    let ``closure conversion generates deterministic lifted method snapshot for identical input`` () =
        let span = { left = Position.Zero; right = Position.Zero }
        let methodSym = SymbolId 300
        let argSid = SymbolId 301
        let scope = Scope(None)
        scope.DeclareVar("main", methodSym)

        // 同一入力を2回前処理し、生成メソッド列のシンボル順が一致することを検証する。
        let lambdaExpr =
            Hir.Expr.Lambda(
                [ Hir.Arg(argSid, "x", TypeId.Int, span) ],
                TypeId.Int,
                Hir.Expr.Id(argSid, TypeId.Int, span),
                TypeId.Fn([ TypeId.Int ], TypeId.Int),
                span)
        let hirMethod = Hir.Method(methodSym, [], lambdaExpr, TypeId.Fn([], TypeId.Fn([ TypeId.Int ], TypeId.Int)), span)
        let hirAssembly = Hir.Assembly("test", [ Hir.Module("Main", [], [], [ hirMethod ], scope) ])

        let snapshotOf (asm: Hir.Assembly) =
            asm.modules.Head.methods
            |> List.map (fun m -> $"{m.sym.id}:{m.args |> List.length}:{m.typ}")
            |> String.concat "|"

        let runOnce () =
            match ClosureConversion.preprocessAssembly hirAssembly with
            | { succeeded = true; value = Some preprocessed } -> snapshotOf preprocessed
            | { diagnostics = diagnostics } ->
                let message = diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
                failwith $"preprocessAssembly failed: {message}"

        let first = runOnce ()
        let second = runOnce ()
        Assert.Equal(first, second)

    [<Fact>]
    let ``captured lambda diagnostic is deterministic and includes sorted captured ids`` () =
        let span = { left = Position.Zero; right = Position.Zero }
        let methodSym = SymbolId 400
        let capturedA = SymbolId 401
        let capturedB = SymbolId 402
        let scope = Scope(None)
        scope.DeclareVar("main", methodSym)
        scope.DeclareVar("a", capturedA)
        scope.DeclareVar("b", capturedB)

        // 逆順で式を配置しても、診断の captured は昇順（401, 402）になることを確認する。
        let lambdaExpr =
            Hir.Expr.Lambda(
                [ Hir.Arg(SymbolId 403, "x", TypeId.Int, span) ],
                TypeId.Int,
                Hir.Expr.Block(
                    [ Hir.Stmt.ExprStmt(Hir.Expr.Id(capturedB, TypeId.Int, span), span) ],
                    Hir.Expr.Id(capturedA, TypeId.Int, span),
                    TypeId.Int,
                    span),
                TypeId.Fn([ TypeId.Int ], TypeId.Int),
                span)
        let hirMethod = Hir.Method(methodSym, [], lambdaExpr, TypeId.Fn([], TypeId.Fn([ TypeId.Int ], TypeId.Int)), span)
        let result = Layout.layoutAssembly("TestAsm", Hir.Assembly("test", [ Hir.Module("Main", [], [], [ hirMethod ], scope) ]))

        Assert.False(result.succeeded, "captured lambda should still fail before env-class implementation")
        let message = result.diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
        Assert.Contains("captured=[401, 402]", message)
