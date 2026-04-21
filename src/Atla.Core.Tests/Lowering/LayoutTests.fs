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
        Assert.Contains("mutable=[]", message)

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

    [<Fact>]
    let ``captured lambda diagnostic reports mutable captured symbol ids`` () =
        let span = { left = Position.Zero; right = Position.Zero }
        let methodSym = SymbolId 500
        let mutableSym = SymbolId 501
        let scope = Scope(None)
        scope.DeclareVar("main", methodSym)

        // var x = 1; (fn _ -> x) という形を HIR で直接構築し、mutable 捕捉が診断に現れることを検証する。
        let lambdaExpr =
            Hir.Expr.Lambda(
                [ Hir.Arg(SymbolId 502, "_", TypeId.Unit, span) ],
                TypeId.Int,
                Hir.Expr.Id(mutableSym, TypeId.Int, span),
                TypeId.Fn([ TypeId.Unit ], TypeId.Int),
                span)

        let body =
            Hir.Expr.Block(
                [ Hir.Stmt.Let(mutableSym, true, Hir.Expr.Int(1, span), span) ],
                lambdaExpr,
                TypeId.Fn([ TypeId.Unit ], TypeId.Int),
                span)

        let hirMethod = Hir.Method(methodSym, [], body, TypeId.Fn([], TypeId.Fn([ TypeId.Unit ], TypeId.Int)), span)
        let result = Layout.layoutAssembly("TestAsm", Hir.Assembly("test", [ Hir.Module("Main", [], [], [ hirMethod ], scope) ]))

        Assert.False(result.succeeded, "captured mutable lambda should fail before env-class implementation")
        let message = result.diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
        Assert.Contains("captured=[501]", message)
        Assert.Contains("mutable=[501]", message)

    [<Fact>]
    let ``for loop variable captured by inner lambda is flagged as captured lambda diagnostic`` () =
        // for 反復変数をラムダ内で参照すると「捕捉ラムダ」として診断が返ることを検証する。
        // C#互換の「反復ごと新規束縛」セマンティクス：lambda が for-loop 変数を使う場合は env-class が必要。
        let span = { left = Position.Zero; right = Position.Zero }
        let methodSym = SymbolId 600
        let iterSym = SymbolId 601
        let lambdaArgSym = SymbolId 602
        let scope = Scope(None)
        scope.DeclareVar("main", methodSym)

        // for i in ...; fn _ -> i という形を HIR で直接構築する。
        let lambdaExpr =
            Hir.Expr.Lambda(
                [ Hir.Arg(lambdaArgSym, "_", TypeId.Unit, span) ],
                TypeId.Int,
                Hir.Expr.Id(iterSym, TypeId.Int, span),
                TypeId.Fn([ TypeId.Unit ], TypeId.Int),
                span)

        // iterable はダミーの Unit 式（実際の MIR 変換は実施しない）。
        let body =
            Hir.Expr.Block(
                [ Hir.Stmt.For(
                    iterSym,
                    TypeId.Int,
                    Hir.Expr.Unit(span),
                    [ Hir.Stmt.ExprStmt(lambdaExpr, span) ],
                    span) ],
                Hir.Expr.Unit(span),
                TypeId.Unit,
                span)

        let hirMethod = Hir.Method(methodSym, [], body, TypeId.Fn([], TypeId.Unit), span)
        let result = Layout.layoutAssembly("TestAsm", Hir.Assembly("test", [ Hir.Module("Main", [], [], [ hirMethod ], scope) ]))

        // 反復変数 601 は for-loop ボディ内で束縛されているが、lambda 内部から見ると捕捉対象となる。
        Assert.False(result.succeeded, "lambda capturing a for-loop variable should fail before env-class implementation")
        let message = result.diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
        Assert.Contains("captured=[601]", message)

    // ─────────────────────────────────────────────────────────────
    // AST/HIR/MIR スナップショット + 診断検証テスト（Task 3: line 739）
    // ─────────────────────────────────────────────────────────────

    /// 単純な整数加算関数の型表現が AST→HIR→MIR を通じて安定していることを検証するスナップショットテスト。
    [<Fact>]
    let ``HIR and MIR type snapshot is stable for simple int addition function`` () =
        let program = "fn add (x: Int) (y: Int): Int = x + y"
        let input: Input<SourceChar> = StringInput program

        let snapshotTypeId (tid: TypeId) : string =
            match tid with
            | TypeId.Int -> "Int"
            | TypeId.Unit -> "Unit"
            | TypeId.Bool -> "Bool"
            | TypeId.String -> "String"
            | TypeId.Fn (args, ret) ->
                let argStr = args |> List.map (fun t -> sprintf "%A" t) |> String.concat ","
                sprintf "Fn([%s],%s)" argStr (sprintf "%A" ret)
            | other -> sprintf "%A" other

        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Success (moduleAst, _) ->
                // AST スナップショット: 引数名の確認。
                match moduleAst.decls with
                | [ (:? Ast.Decl.Fn as fnDecl) ] ->
                    Assert.Equal("add", fnDecl.name)
                    Assert.Equal(2, fnDecl.args.Length)
                | _ -> Assert.True(false, "expected single fn decl")

                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", moduleAst) with
                | { succeeded = true; value = Some hirModule } ->
                    // HIR スナップショット: add メソッドの型は Fn([Int,Int],Int) であるべき。
                    let addMethod = hirModule.methods |> List.tryFind (fun m -> m.sym.id = hirModule.scope.vars.["add"].id)
                    match addMethod with
                    | Some meth ->
                        match Type.resolve subst meth.typ with
                        | TypeId.Fn ([ TypeId.Int; TypeId.Int ], TypeId.Int) ->
                            Assert.True(true)
                        | other ->
                            Assert.True(false, sprintf "HIR add method type mismatch: %A" other)
                    | None -> Assert.True(false, "HIR add method not found")

                    // MIR スナップショット: add の MIR メソッドが args/ret を正しく持つことを確認。
                    let mirResult = Layout.layoutAssembly("TestAsm", Hir.Assembly("test", [ hirModule ]))
                    match mirResult with
                    | { succeeded = true; value = Some mirAsm } ->
                        let mirAdd = mirAsm.modules.Head.methods |> List.tryFind (fun m -> m.name = "add")
                        match mirAdd with
                        | Some m ->
                            Assert.Equal<TypeId list>([ TypeId.Int; TypeId.Int ], m.args)
                            Assert.Equal(TypeId.Int, m.ret)
                        | None -> Assert.True(false, "MIR add method not found")
                    | { diagnostics = diagnostics } ->
                        let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                        Assert.True(false, sprintf "layoutAssembly failed: %s" message)
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun err -> err.toDisplayText()) |> String.concat "; "
                    Assert.True(false, sprintf "Semantic analysis failed: %s" message)
            | Failure (reason, span) ->
                Assert.True(false, sprintf "Parsing failed: %s at %d:%d" reason span.left.Line span.left.Column)
        | Failure (reason, span) ->
            Assert.True(false, sprintf "Lexing failed: %s at %d:%d" reason span.left.Line span.left.Column)

    /// 未定義変数を参照するプログラムの診断メッセージが安定していることを検証するスナップショットテスト。
    [<Fact>]
    let ``diagnostic snapshot is stable for undefined variable reference`` () =
        let program = """
fn bad (): Int = undefinedVar
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
                | { succeeded = false; diagnostics = diagnostics } ->
                    let combined = diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
                    // 未定義変数エラーメッセージが含まれるべき。
                    Assert.Contains("undefinedVar", combined)
                | { succeeded = true } ->
                    Assert.True(false, "program with undefined variable must not succeed")
            | Failure (reason, span) ->
                Assert.True(false, sprintf "Parsing failed: %s at %d:%d" reason span.left.Line span.left.Column)
        | Failure (reason, span) ->
            Assert.True(false, sprintf "Lexing failed: %s at %d:%d" reason span.left.Line span.left.Column)

    /// 同一入力を2回レイアウトしたとき MIR スナップショットが一致することを検証する（決定性テスト）。
    [<Fact>]
    let ``MIR snapshot is deterministic across repeated layoutAssembly calls for identical input`` () =
        let span = { left = Position.Zero; right = Position.Zero }
        let methodSym = SymbolId 700
        let argSid = SymbolId 701
        let scope = Scope(None)
        scope.DeclareVar("compute", methodSym)

        let hirMethod =
            Hir.Method(
                methodSym,
                [ argSid, TypeId.Int ],
                Hir.Expr.Id(argSid, TypeId.Int, span),
                TypeId.Fn([ TypeId.Int ], TypeId.Int),
                span)
        let hirAssembly = Hir.Assembly("test", [ Hir.Module("Main", [], [], [ hirMethod ], scope) ])

        let snapshotOf (result: PhaseResult<Mir.Assembly>) =
            match result with
            | { succeeded = true; value = Some mirAsm } ->
                mirAsm.modules.Head.methods
                |> List.map (fun m -> sprintf "%s|args=%d|ret=%A" m.name m.args.Length m.ret)
                |> String.concat ";"
            | { diagnostics = diagnostics } ->
                let message = diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
                failwith (sprintf "layoutAssembly failed: %s" message)

        let first = snapshotOf (Layout.layoutAssembly("TestAsm", hirAssembly))
        let second = snapshotOf (Layout.layoutAssembly("TestAsm", hirAssembly))
        Assert.Equal(first, second)
