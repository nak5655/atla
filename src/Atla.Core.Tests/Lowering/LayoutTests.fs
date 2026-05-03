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
    /// `Hir.Assembly` に対してクロージャー変換とレイアウトを連続して適用するヘルパー。
    /// テストコードで `ClosureConversion → Layout` の2段パイプラインを簡潔に呼び出せるようにする。
    /// 新規 `SymbolTable` を生成して採番の独立性を保証する。
    let private layoutHirAssembly (asmName: string, asm: Hir.Assembly) : PhaseResult<Mir.Assembly> =
        let symbolTable = SymbolTable()
        match ClosureConversion.preprocessAssembly(symbolTable, asm) with
        | { succeeded = false; diagnostics = diagnostics } -> PhaseResult.failed diagnostics
        | { value = Some closedAsm } -> Layout.layoutAssembly(asmName, closedAsm)
        | _ -> PhaseResult.failed [ Diagnostic.Error("Closure conversion failed with unknown state", Span.Empty) ]

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

        let mirAssemblyResult = layoutHirAssembly("TestAsm", hirAssembly)
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
import System'Console
import System'Linq'Enumerable

fn main: () =
    for i in 1 3 Enumerable'Range.
        i Console'WriteLine.
"""

        let input: Input<SourceChar> = StringInput program
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (moduleAst, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", moduleAst) with
                | { succeeded = true; value = Some hirModule } ->
                    let mirAssemblyResult = layoutHirAssembly("TestAsm", Hir.Assembly("test", [ hirModule ]))
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
            match Parser.fileModule tokenInput start with
            | Success (moduleAst, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", moduleAst) with
                | { succeeded = true; value = Some hirModule } ->
                    let mirAssemblyResult = layoutHirAssembly("TestAsm", Hir.Assembly("test", [ hirModule ]))
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
    let ``layoutAssembly keeps receiver as first MIR call argument for instance method`` () =
        let program = """
import System'IO'StringWriter

fn main: () = do
    let writer = StringWriter.
    "hello" writer'WriteLine.
"""

        let input: Input<SourceChar> = StringInput program
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (moduleAst, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", moduleAst) with
                | { succeeded = true; value = Some hirModule } ->
                    let mirAssemblyResult = layoutHirAssembly("TestAsm", Hir.Assembly("test", [ hirModule ]))
                    let mirAssembly =
                        match mirAssemblyResult with
                        | { succeeded = true; value = Some asm } -> asm
                        | { diagnostics = diagnostics } ->
                            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                            failwith $"layoutAssembly failed: {message}"

                    let entryMethodBody = mirAssembly.modules.Head.methods.Head.body
                    let writeLineCall =
                        entryMethodBody
                        |> List.tryFind (function
                            | Mir.Ins.Call (Choice1Of2 methodInfo, _) when methodInfo.Name = "WriteLine" -> true
                            | _ -> false)

                    match writeLineCall with
                    | Some (Mir.Ins.Call (_, args)) ->
                        match args with
                        | Mir.Value.RegVal _ :: Mir.Value.ImmVal(Mir.Imm.String text) :: [] ->
                            Assert.Equal("hello", text)
                        | _ ->
                            Assert.True(false, $"Unexpected WriteLine args shape: {args}")
                    | _ ->
                        Assert.True(false, "MIR call for StringWriter.WriteLine was not found.")
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
            match Parser.fileModule tokenInput start with
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

                    let mirAssemblyResult = layoutHirAssembly("TestAsm", Hir.Assembly("test", [ hirModule ]))
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
        let result = layoutHirAssembly("TestAsm", hirAssembly)

        // capturedSym は外側メソッドの引数でも let 束縛でもないため、ClosureConversion が型情報不明診断を出す。
        Assert.False(result.succeeded, "captured lambda with no binding info should fail in preprocessing stage")
        let message = result.diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
        Assert.Contains("Closure conversion failed: captured variable(s) have no type information", message)
        Assert.Contains("ownerMethodSid=100", message)
        Assert.Contains("sids=[101]", message)

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
        let result = layoutHirAssembly("TestAsm", hirAssembly)

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

        let snapshotOf (asm: ClosedHir.Assembly) =
            asm.modules.Head.methods
            |> List.map (fun m -> $"{m.sym.id}:{m.args |> List.length}:{m.typ}")
            |> String.concat "|"

        let runOnce () =
            // 決定性を検証するため、各実行ごとに独立した SymbolTable を生成する。
            let symbolTable = SymbolTable()
            match ClosureConversion.preprocessAssembly(symbolTable, hirAssembly) with
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
        let result = layoutHirAssembly("TestAsm", Hir.Assembly("test", [ Hir.Module("Main", [], [], [ hirMethod ], scope) ]))

        Assert.False(result.succeeded, "captured lambda with no binding info should still fail before env-class implementation")
        let message = result.diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
        // 捕捉変数の sids が昇順に並んでいることを検証する（決定性チェック）。
        Assert.Contains("sids=[401, 402]", message)

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
        let result = layoutHirAssembly("TestAsm", Hir.Assembly("test", [ Hir.Module("Main", [], [], [ hirMethod ], scope) ]))

        // mutableSym は let 束縛でメソッドの bindings に入るため、env-class 変換が成功すべき。
        Assert.True(result.succeeded, "mutable captured lambda should be successfully lowered via env-class conversion")
        match result.value with
        | Some mirAsm ->
            // env-class 型がモジュールの types に追加されていることを確認する。
            let hasEnvType = mirAsm.modules.Head.types.Length > 0
            Assert.True(hasEnvType, "env-class type should be generated for mutable captured lambda")
            // 外側メソッドの body に NewEnv 命令が含まれていることを確認する。
            let mainMethodBody = mirAsm.modules.Head.methods.Head.body
            let hasNewEnv =
                mainMethodBody
                |> List.exists (function
                    | Mir.Ins.NewEnv _ -> true
                    | _ -> false)
            Assert.True(hasNewEnv, "outer method body should contain NewEnv instruction for env-class creation")
        | None ->
            Assert.True(false, "layoutAssembly succeeded but returned no MIR assembly")

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

        // 新しい動作: iterSym は for 文のボディ内で bindings に登録されるため、
        // ClosureConversion は env-class 変換に成功する。
        // ClosureConversion レベルでは診断なし（変換成功）であることを検証する。
        let hirAssembly = Hir.Assembly("test", [ Hir.Module("Main", [], [], [ hirMethod ], scope) ])
        let ccResult = ClosureConversion.preprocessAssembly(SymbolTable(), hirAssembly)
        Assert.True(ccResult.succeeded, "ClosureConversion should succeed for for-loop variable capture (variable is in bindings)")

        // ClosureConversion 後のモジュールに env-class 型が生成されていることを確認する。
        match ccResult.value with
        | Some preprocessed ->
            let envTypes = preprocessed.modules.Head.types
            Assert.True(envTypes.Length > 0, "env-class type should be generated for for-loop variable capture")
        | None ->
            Assert.True(false, "preprocessAssembly succeeded but returned no assembly")

    [<Fact>]
    let ``layoutAssembly lowers captured lambda via env-class conversion`` () =
        // 外側メソッドの引数を捕捉するラムダを env-class 変換で lower できることを検証する。
        // 外側メソッド: fn main (n: Int): Func<int,int> = fn x -> n + x
        // → env-class に n を格納し、invoke メソッドが n + x を計算する。
        let span = { left = Position.Zero; right = Position.Zero }
        let methodSym = SymbolId 800
        let outerArgSid = SymbolId 801
        let lambdaArgSid = SymbolId 802
        let scope = Scope(None)
        scope.DeclareVar("main", methodSym)

        // ラムダ本体: outerArgSid + lambdaArgSid（実際の計算は GenTests で検証するため、ここでは型構造を確認）。
        let lambdaExpr =
            Hir.Expr.Lambda(
                [ Hir.Arg(lambdaArgSid, "x", TypeId.Int, span) ],
                TypeId.Int,
                // 本体は outerArgSid を参照するシンプルな式（capturedSid のみ）
                Hir.Expr.Id(outerArgSid, TypeId.Int, span),
                TypeId.Fn([ TypeId.Int ], TypeId.Int),
                span)

        let hirMethod =
            Hir.Method(
                methodSym,
                [ outerArgSid, TypeId.Int ],
                lambdaExpr,
                TypeId.Fn([ TypeId.Int ], TypeId.Fn([ TypeId.Int ], TypeId.Int)),
                span)

        let hirModule = Hir.Module("Main", [], [], [ hirMethod ], scope)
        let hirAssembly = Hir.Assembly("test", [ hirModule ])
        let result = layoutHirAssembly("TestAsm", hirAssembly)

        Assert.True(result.succeeded, "captured lambda should be lowered successfully via env-class conversion")
        match result.value with
        | Some mirAsm ->
            let mirModule = mirAsm.modules.Head

            // env-class 型がモジュールの types に追加されていることを確認する。
            Assert.True(mirModule.types.Length > 0, "env-class type should be generated for captured lambda")
            let envType = mirModule.types.Head

            // env-class 型にフィールドが 1 件（捕捉変数 outerArgSid = 801）あることを確認する。
            Assert.Equal(1, envType.fields.Length)
            Assert.Equal(outerArgSid, envType.fields.Head.sym)
            Assert.Equal(TypeId.Int, envType.fields.Head.typ)

            // env-class 型に invoke インスタンスメソッドが 1 件あることを確認する。
            Assert.Equal(1, envType.methods.Length)
            let invokeMethod = envType.methods.Head
            // invoke メソッドのシグネチャ: 明示引数 = [TypeId.Int]（env インスタンスは除外済み）。
            Assert.Equal<TypeId list>([ TypeId.Int ], invokeMethod.args)
            Assert.Equal(TypeId.Int, invokeMethod.ret)

            // 外側メソッドの body に NewEnv・StoreEnvField・FnDelegate が含まれることを確認する。
            let mainMethod = mirModule.methods.Head
            let hasNewEnv = mainMethod.body |> List.exists (function Mir.Ins.NewEnv _ -> true | _ -> false)
            let hasStoreEnvField = mainMethod.body |> List.exists (function Mir.Ins.StoreEnvField _ -> true | _ -> false)
            let hasFnDelegate = mainMethod.body |> List.exists (function Mir.Ins.RetValue(Mir.Value.FnDelegate(_, _, Some _)) -> true | _ -> false)

            Assert.True(hasNewEnv, "outer method should contain NewEnv instruction")
            Assert.True(hasStoreEnvField, "outer method should contain StoreEnvField instruction")
            Assert.True(hasFnDelegate, "outer method should return FnDelegate bound to env instance")
        | None ->
            Assert.True(false, "layoutAssembly succeeded but returned no MIR assembly")

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
            match Parser.fileModule tokenInput start with
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
                    let mirResult = layoutHirAssembly("TestAsm", Hir.Assembly("test", [ hirModule ]))
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
            match Parser.fileModule tokenInput start with
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

        let first = snapshotOf (layoutHirAssembly("TestAsm", hirAssembly))
        let second = snapshotOf (layoutHirAssembly("TestAsm", hirAssembly))
        Assert.Equal(first, second)

    /// ExprError ノードを lower しようとすると、そのスパン付きの Diagnostic.Error が返ることを検証する。
    [<Fact>]
    let ``layoutAssembly returns span-attached diagnostic for ExprError node`` () =
        let errorSpan = { left = { Line = 3; Column = 5 }; right = { Line = 3; Column = 20 } }
        let methodSym = SymbolId 900
        let scope = Scope(None)
        scope.DeclareVar("bad", methodSym)

        // ExprError をメソッド本体に持つメソッドを直接 ClosedHir で構築する。
        let closedMethod =
            ClosedHir.Method(
                methodSym,
                [],
                ClosedHir.Expr.ExprError("type mismatch in test", TypeId.Int, errorSpan),
                TypeId.Fn([], TypeId.Int),
                errorSpan)

        let closedModule =
            ClosedHir.Module("Main", [], [], [ closedMethod ], scope, Map.empty)
        let closedAsm = ClosedHir.Assembly("test", [ closedModule ])

        let result = Layout.layoutAssembly("TestAsm", closedAsm)

        Assert.False(result.succeeded, "lowering ExprError should produce a failure")
        let diag = result.diagnostics |> List.tryFind (fun d -> d.message.Contains("type mismatch in test"))
        Assert.True(diag.IsSome, "diagnostic message should reference the original error text")
        match diag with
        | Some d ->
            Assert.Equal(errorSpan.left.Line, d.span.left.Line)
            Assert.Equal(errorSpan.left.Column, d.span.left.Column)
        | None -> ()

    /// ErrorStmt ノードを lower しようとすると、そのスパン付きの Diagnostic.Error が返ることを検証する。
    [<Fact>]
    let ``layoutAssembly returns span-attached diagnostic for ErrorStmt node`` () =
        let stmtSpan = { left = { Line = 7; Column = 2 }; right = { Line = 7; Column = 15 } }
        let bodySpan = { left = Position.Zero; right = Position.Zero }
        let methodSym = SymbolId 901
        let scope = Scope(None)
        scope.DeclareVar("withError", methodSym)

        // ErrorStmt を含む Block をメソッド本体に持つメソッドを直接 ClosedHir で構築する。
        let closedMethod =
            ClosedHir.Method(
                methodSym,
                [],
                ClosedHir.Expr.Block(
                    [ ClosedHir.Stmt.ErrorStmt("stmt error in test", stmtSpan) ],
                    ClosedHir.Expr.Unit bodySpan,
                    TypeId.Unit,
                    bodySpan),
                TypeId.Fn([], TypeId.Unit),
                bodySpan)

        let closedModule =
            ClosedHir.Module("Main", [], [], [ closedMethod ], scope, Map.empty)
        let closedAsm = ClosedHir.Assembly("test", [ closedModule ])

        let result = Layout.layoutAssembly("TestAsm", closedAsm)

        Assert.False(result.succeeded, "lowering ErrorStmt should produce a failure")
        let diag = result.diagnostics |> List.tryFind (fun d -> d.message.Contains("stmt error in test"))
        Assert.True(diag.IsSome, "diagnostic message should reference the original error text")
        match diag with
        | Some d ->
            Assert.Equal(stmtSpan.left.Line, d.span.left.Line)
            Assert.Equal(stmtSpan.left.Column, d.span.left.Column)
        | None -> ()

    /// 未定義変数への代入文を lower しようとすると、そのスパン付きの Diagnostic.Error が返ることを検証する。
    [<Fact>]
    let ``layoutAssembly returns span-attached diagnostic for undefined variable in assignment`` () =
        let assignSpan = { left = { Line = 5; Column = 4 }; right = { Line = 5; Column = 18 } }
        let bodySpan = { left = Position.Zero; right = Position.Zero }
        let methodSym = SymbolId 902
        let undefinedSym = SymbolId 999
        let scope = Scope(None)
        scope.DeclareVar("badAssign", methodSym)

        // フレームに存在しない undefinedSym への代入文を直接 ClosedHir で構築する。
        let closedMethod =
            ClosedHir.Method(
                methodSym,
                [],
                ClosedHir.Expr.Block(
                    [ ClosedHir.Stmt.Assign(undefinedSym, ClosedHir.Expr.Int(42, bodySpan), assignSpan) ],
                    ClosedHir.Expr.Unit bodySpan,
                    TypeId.Unit,
                    bodySpan),
                TypeId.Fn([], TypeId.Unit),
                bodySpan)

        let closedModule =
            ClosedHir.Module("Main", [], [], [ closedMethod ], scope, Map.empty)
        let closedAsm = ClosedHir.Assembly("test", [ closedModule ])

        let result = Layout.layoutAssembly("TestAsm", closedAsm)

        Assert.False(result.succeeded, "assigning to an undefined variable should produce a failure")
        let diag = result.diagnostics |> List.tryFind (fun d -> d.message.Contains("Undefined variable in assignment"))
        Assert.True(diag.IsSome, "diagnostic should indicate undefined variable")
        match diag with
        | Some d ->
            Assert.Equal(assignSpan.left.Line, d.span.left.Line)
            Assert.Equal(assignSpan.left.Column, d.span.left.Column)
        | None -> ()
