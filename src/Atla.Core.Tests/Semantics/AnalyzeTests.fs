namespace Atla.Core.Tests.Semantics

open Xunit
open Atla.Core.Data
open Atla.Core.Syntax
open Atla.Core.Syntax.Data
open Atla.Core.Semantics
open Atla.Core.Semantics.Data
open Atla.Core.Lowering

module AnalyzeTests =
    [<Fact>]
    let ``analyzeMethod infers argument reference type`` () =
        let span = Span.Empty
        let argType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let retType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let arg = Ast.FnArg.Named("x", argType, span) :> Ast.FnArg
        let body = Ast.Expr.Id("x", span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("id", [arg], retType, body, span) :> Ast.Decl
        let astModule = Ast.Module([fnDecl])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            match hirModule.methods.Head.body with
            | Hir.Expr.Id (_, typ, _) -> Assert.Equal(TypeId.Int, Type.resolve subst typ)
            | other -> Assert.True(false, $"expected Hir.Expr.Id but got {other}")
        | { diagnostics = diagnostics } ->
            let message =
                diagnostics
                |> List.map (fun err -> err.toDisplayText())
                |> String.concat "; "
            Assert.True(false, $"semantic analysis failed: {message}")

    [<Fact>]
    let ``ast to hir should not keep error nodes`` () =
        let span = Span.Empty
        let importDecl = Ast.Decl.Import([ "System"; "Console" ], span) :> Ast.Decl
        let retType = Ast.TypeExpr.Unit(span) :> Ast.TypeExpr
        let writeLineExpr = Ast.Expr.StaticAccess("Console", "WriteLine", span) :> Ast.Expr
        let helloArg = Ast.Expr.String("Hello, World!", span) :> Ast.Expr
        let callExpr = Ast.Expr.Apply(writeLineExpr, [ helloArg ], span) :> Ast.Expr
        let bodyStmt = Ast.Stmt.ExprStmt(callExpr, span) :> Ast.Stmt
        let body = Ast.Expr.Block([ bodyStmt ], span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, span) :> Ast.Decl
        let astModule = Ast.Module([ importDecl; fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            let hasError =
                hirModule.methods
                |> List.exists (fun m -> m.hasError)

            Assert.False(hasError, "HIR に ExprError/ErrorStmt が残っています。")
        | { diagnostics = diagnostics } ->
            let message =
                diagnostics
                |> List.map (fun err -> err.toDisplayText())
                |> String.concat "; "
            Assert.True(false, $"semantic analysis failed: {message}")

    [<Fact>]
    let ``semantic analysis and lowering handle do block`` () =
        let program = """
fn main (): Int = do
    let value = 1
    value
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
                    let hirAsm = Hir.Assembly("test", [ hirModule ])
                    match ClosureConversion.preprocessAssembly(symbolTable, hirAsm) with
                    | { succeeded = true; value = Some closedAsm } ->
                        match Layout.layoutAssembly("TestAsm", closedAsm) with
                        | { succeeded = true; value = Some mirAsm } -> Assert.Single(mirAsm.modules) |> ignore
                        | { diagnostics = diagnostics } ->
                            let message =
                                diagnostics
                                |> List.map (fun err -> err.toDisplayText())
                                |> String.concat "; "
                            Assert.True(false, $"Lowering failed: {message}")
                    | { diagnostics = diagnostics } ->
                        let message =
                            diagnostics
                            |> List.map (fun err -> err.toDisplayText())
                            |> String.concat "; "
                        Assert.True(false, $"Closure conversion failed: {message}")
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
    let ``semantic analysis preserves Ast.Expr.Error message`` () =
        let span = Span.Empty
        let retType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let body = Ast.Expr.Error("Expected '.' after callee in call expression.", span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, span) :> Ast.Decl
        let astModule = Ast.Module([ fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        let diagnostics =
            match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
            | { succeeded = true; value = Some hirModule } ->
                hirModule.methods |> List.collect (fun methodInfo -> methodInfo.getDiagnostics)
            | { diagnostics = diagnostics } -> diagnostics

        Assert.Contains(diagnostics, fun diagnostic -> diagnostic.message.Contains("Expected '.' after callee in call expression."))
        Assert.DoesNotContain(diagnostics, fun diagnostic -> diagnostic.message.Contains("Unsupported expression type"))

    [<Fact>]
    let ``semantic analysis preserves dangling apostrophe parser error message`` () =
        let program = "fn main (): Int = value'"
        let input: Input<SourceChar> = StringInput program

        let diagnostics =
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
                        hirModule.methods |> List.collect (fun methodInfo -> methodInfo.getDiagnostics)
                    | { diagnostics = diagnostics } -> diagnostics
                | Failure (reason, span) ->
                    [ Diagnostic.Error($"Parsing failed: {reason}", span) ]
            | Failure (reason, span) ->
                [ Diagnostic.Error($"Lexing failed: {reason}", span) ]

        Assert.Contains(diagnostics, fun diagnostic -> diagnostic.message.Contains("Expected member identifier after apostrophe."))
        Assert.DoesNotContain(diagnostics, fun diagnostic -> diagnostic.message.Contains("Unsupported expression type"))

    [<Fact>]
    let ``semantic analysis keeps canonical Hir.Call shape for function apply`` () =
        let span = Span.Empty
        let intType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let incArg = Ast.FnArg.Named("x", intType, span) :> Ast.FnArg
        let incBody = Ast.Expr.Id("x", span) :> Ast.Expr
        let incDecl = Ast.Decl.Fn("inc", [ incArg ], intType, incBody, span) :> Ast.Decl
        let mainBody =
            Ast.Expr.Apply(
                Ast.Expr.Id("inc", span) :> Ast.Expr,
                [ Ast.Expr.Int(1, span) :> Ast.Expr ],
                span
            ) :> Ast.Expr
        let mainDecl = Ast.Decl.Fn("main", [], intType, mainBody, span) :> Ast.Decl
        let astModule = Ast.Module([ incDecl; mainDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            let mainMethod =
                hirModule.methods
                |> List.tryFind (fun methodInfo -> symbolTable.Get(methodInfo.sym) |> Option.exists (fun symbolInfo -> symbolInfo.name = "main"))

            match mainMethod with
            | Some methodInfo ->
                match methodInfo.body with
                | Hir.Expr.Call (Hir.Callable.Fn _, _, [ Hir.Expr.Int (1, _) ], _, _) ->
                    Assert.True(true)
                | other ->
                    Assert.True(false, $"expected canonical Hir.Expr.Call for function apply but got {other}")
            | None ->
                Assert.True(false, "main method was not found in analyzed HIR module")
        | { diagnostics = diagnostics } ->
            let message =
                diagnostics
                |> List.map (fun err -> err.toDisplayText())
                |> String.concat "; "
            Assert.True(false, $"Semantic analysis failed: {message}")

    [<Fact>]
    let ``semantic analysis lowers dot-only member call into Hir.Call with native callable`` () =
        let program = """
import System'Console
fn main (): () = "hello" Console'WriteLine.
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
                    let mainMethod =
                        hirModule.methods
                        |> List.tryFind (fun methodInfo -> symbolTable.Get(methodInfo.sym) |> Option.exists (fun symbolInfo -> symbolInfo.name = "main"))

                    match mainMethod with
                    | Some methodInfo ->
                        match methodInfo.body with
                        | Hir.Expr.Call (Hir.Callable.NativeMethod _, _, [ Hir.Expr.String ("hello", _) ], _, _) ->
                            Assert.True(true)
                        | other ->
                            Assert.True(false, $"expected native Hir.Expr.Call for dot-only member call but got {other}")
                    | None ->
                        Assert.True(false, "main method was not found in analyzed HIR module")
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
    let ``semantic analysis and lowering accept data declaration and named initialization`` () =
        let program = """
data Person = { name: String, age: Int }
fn buildPerson (): Person = Person { name = "Alice", age = 20 }
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
                    let hirAsm = Hir.Assembly("test", [ hirModule ])
                    match ClosureConversion.preprocessAssembly(symbolTable, hirAsm) with
                    | { succeeded = true; value = Some closedAsm } ->
                        match Layout.layoutAssembly("TestAsm", closedAsm) with
                        | { succeeded = true; value = Some mirAsm } ->
                            Assert.Single(mirAsm.modules.Head.types) |> ignore
                        | { diagnostics = diagnostics } ->
                            let message =
                                diagnostics
                                |> List.map (fun err -> err.toDisplayText())
                                |> String.concat "; "
                            Assert.True(false, $"Lowering failed: {message}")
                    | { diagnostics = diagnostics } ->
                        let message =
                            diagnostics
                            |> List.map (fun err -> err.toDisplayText())
                            |> String.concat "; "
                        Assert.True(false, $"Closure conversion failed: {message}")
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
    let ``semantic analysis accepts impl method with explicit this and data member access`` () =
        let span = Span.Empty
        let floatType = Ast.TypeExpr.Id("Float", span) :> Ast.TypeExpr
        let lineType = Ast.TypeExpr.Id("Line", span) :> Ast.TypeExpr
        let dataDecl =
            Ast.Decl.Data(
                "Line",
                [ Ast.DataItem.Field("slope", floatType, span) :> Ast.DataItem
                  Ast.DataItem.Field("intercept", floatType, span) :> Ast.DataItem ],
                span) :> Ast.Decl

        let thisArg = Ast.FnArg.Named("this", lineType, span) :> Ast.FnArg
        let xArg = Ast.FnArg.Named("x", floatType, span) :> Ast.FnArg
        let evalBody = Ast.Expr.MemberAccess(Ast.Expr.Id("this", span) :> Ast.Expr, "slope", span) :> Ast.Expr
        let evalFn = Ast.Decl.Fn("evaluate", [ thisArg; xArg ], floatType, evalBody, span)
        let implDecl = Ast.Decl.Impl("Line", None, None, [ evalFn ], span) :> Ast.Decl

        let lineInit =
            Ast.Expr.DataInit(
                "Line",
                [ Ast.DataInitField.Field("slope", Ast.Expr.Float(2.0, span) :> Ast.Expr, span) :> Ast.DataInitField
                  Ast.DataInitField.Field("intercept", Ast.Expr.Float(-1.0, span) :> Ast.Expr, span) :> Ast.DataInitField ],
                span) :> Ast.Expr
        let callBody =
            Ast.Expr.Apply(
                Ast.Expr.MemberAccess(lineInit, "evaluate", span) :> Ast.Expr,
                [ Ast.Expr.Float(5.0, span) :> Ast.Expr ],
                span) :> Ast.Expr
        let mainDecl = Ast.Decl.Fn("main", [], floatType, callBody, span) :> Ast.Decl

        let astModule = Ast.Module([ dataDecl; implDecl; mainDecl ])
        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            let hasEvaluate =
                hirModule.methods
                |> List.exists (fun methodInfo -> symbolTable.Get(methodInfo.sym) |> Option.exists (fun symbolInfo -> symbolInfo.name = "Line.evaluate"))
            Assert.True(hasEvaluate, "impl method symbol 'Line.evaluate' was not found")
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            Assert.True(false, $"Semantic analysis failed: {message}")

    [<Fact>]
    let ``semantic analysis accepts impl method without this as static member`` () =
        let span = Span.Empty
        let intType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let dataDecl =
            Ast.Decl.Data(
                "Line",
                [ Ast.DataItem.Field("value", intType, span) :> Ast.DataItem ],
                span) :> Ast.Decl

        let oneFn = Ast.Decl.Fn("one", [], intType, Ast.Expr.Int(1, span) :> Ast.Expr, span)
        let implDecl = Ast.Decl.Impl("Line", None, None, [ oneFn ], span) :> Ast.Decl
        let staticCall = Ast.Expr.Apply(Ast.Expr.MemberAccess(Ast.Expr.Id("Line", span) :> Ast.Expr, "one", span) :> Ast.Expr, [], span) :> Ast.Expr
        let mainDecl = Ast.Decl.Fn("main", [], intType, staticCall, span) :> Ast.Decl

        let astModule = Ast.Module([ dataDecl; implDecl; mainDecl ])
        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            let hasOne =
                hirModule.methods
                |> List.exists (fun methodInfo -> symbolTable.Get(methodInfo.sym) |> Option.exists (fun symbolInfo -> symbolInfo.name = "Line.one"))
            Assert.True(hasOne, "impl static method symbol 'Line.one' was not found")
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            Assert.True(false, $"Semantic analysis failed: {message}")

    [<Fact>]
    let ``semantic analysis reports instance access to static impl method`` () =
        let span = Span.Empty
        let intType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let dataDecl =
            Ast.Decl.Data(
                "Line",
                [ Ast.DataItem.Field("value", intType, span) :> Ast.DataItem ],
                span) :> Ast.Decl

        let oneFn = Ast.Decl.Fn("one", [], intType, Ast.Expr.Int(1, span) :> Ast.Expr, span)
        let implDecl = Ast.Decl.Impl("Line", None, None, [ oneFn ], span) :> Ast.Decl
        let lineInit =
            Ast.Expr.DataInit(
                "Line",
                [ Ast.DataInitField.Field("value", Ast.Expr.Int(42, span) :> Ast.Expr, span) :> Ast.DataInitField ],
                span) :> Ast.Expr
        let badCall = Ast.Expr.Apply(Ast.Expr.MemberAccess(lineInit, "one", span) :> Ast.Expr, [], span) :> Ast.Expr
        let mainDecl = Ast.Decl.Fn("main", [], intType, badCall, span) :> Ast.Decl

        let astModule = Ast.Module([ dataDecl; implDecl; mainDecl ])
        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = false } ->
            Assert.True(true)
        | _ ->
            Assert.True(false, "Semantic analysis unexpectedly succeeded for instance access to static impl method")

    [<Fact>]
    let ``semantic analysis reports cyclic subtype relation declared by impl for`` () =
        let source = """
data A = { value: Int }
data B = { value: Int }
impl A for B
    fn asInt (this: A): Int = this'value
impl B for A
    fn asInt (this: B): Int = this'value
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = false; diagnostics = diagnostics } ->
                    let hasCycleDiagnostic =
                        diagnostics
                        |> List.exists (fun d -> d.toDisplayText().Contains("Cyclic subtype relation detected"))
                    Assert.True(hasCycleDiagnostic, "Expected cyclic subtype relation diagnostic was not reported")
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for cyclic subtype relation")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis resolves delegated native member via impl for by`` () =
        let source = """
import System'DateTime
data Clock = { dt: DateTime }
impl Clock for DateTime by dt
fn year (c: Clock): Int = c'Year
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    let yearMethod =
                        hirModule.methods
                        |> List.tryFind (fun methodDef ->
                            match symbolTable.Get(methodDef.sym) with
                            | Some symInfo -> symInfo.name = "year"
                            | None -> false)
                    Assert.True(yearMethod.IsSome, "Expected delegated-member consumer method 'year' to be analyzed successfully")
                | { diagnostics = diagnostics } ->
                    let message =
                        diagnostics
                        |> List.map (fun diagnostic -> diagnostic.toDisplayText())
                        |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed unexpectedly: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis allows multiple impl for blocks when roles differ`` () =
        let source = """
data Shape = { value: Int }
data Reader = { marker: Int }
data Writer = { marker: Int }
impl Shape for Reader
    fn read (this: Shape): Int = this'value
impl Shape for Writer
    fn write (this: Shape): Int = this'value
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some _ } ->
                    Assert.True(true)
                | { diagnostics = diagnostics } ->
                    let message =
                        diagnostics
                        |> List.map (fun diagnostic -> diagnostic.toDisplayText())
                        |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed unexpectedly: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis reports duplicate default impl blocks`` () =
        let source = """
data Line = { value: Int }
impl Line
    fn first (this: Line): Int = this'value
impl Line
    fn second (this: Line): Int = this'value
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = false; diagnostics = diagnostics } ->
                    let hasExpectedDiagnostic =
                        diagnostics
                        |> List.exists (fun diagnostic -> diagnostic.message.Contains("already has a default impl block"))
                    Assert.True(hasExpectedDiagnostic, "Expected duplicate default impl diagnostic was not reported")
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for duplicate default impl blocks")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis reports duplicate impl blocks for same role`` () =
        let source = """
data Line = { value: Int }
data Reader = { marker: Int }
impl Line for Reader
    fn first (this: Line): Int = this'value
impl Line for Reader
    fn second (this: Line): Int = this'value
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = false; diagnostics = diagnostics } ->
                    let hasExpectedDiagnostic =
                        diagnostics
                        |> List.exists (fun diagnostic -> diagnostic.message.Contains("already has an impl block for role 'Reader'"))
                    Assert.True(hasExpectedDiagnostic, "Expected duplicate role impl diagnostic was not reported")
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for duplicate role impl blocks")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis handles chained dot-only calls`` () =
        let span = Span.Empty
        let intType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let mkArg name = Ast.FnArg.Named(name, intType, span) :> Ast.FnArg
        let idDecl = Ast.Decl.Fn("id", [ mkArg "x" ], intType, Ast.Expr.Id("x", span) :> Ast.Expr, span) :> Ast.Decl
        let incDecl = Ast.Decl.Fn("inc", [ mkArg "y" ], intType, Ast.Expr.Id("y", span) :> Ast.Expr, span) :> Ast.Decl

        let mainBody =
            Ast.Expr.Apply(
                Ast.Expr.Id("inc", span) :> Ast.Expr,
                [ Ast.Expr.Apply(Ast.Expr.Id("id", span) :> Ast.Expr, [ Ast.Expr.Int(1, span) :> Ast.Expr ], span) :> Ast.Expr ],
                span
            ) :> Ast.Expr
        let mainDecl = Ast.Decl.Fn("main", [], intType, mainBody, span) :> Ast.Decl
        let astModule = Ast.Module([ idDecl; incDecl; mainDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            let diagnostics = hirModule.methods |> List.collect (fun methodInfo -> methodInfo.getDiagnostics)
            Assert.Empty(diagnostics)
        | { diagnostics = diagnostics } ->
            let message =
                diagnostics
                |> List.map (fun err -> err.toDisplayText())
                |> String.concat "; "
            Assert.True(false, $"Semantic analysis failed: {message}")

    [<Fact>]
    let ``semantic analysis handles multi argument dot-only call`` () =
        let program = """
fn add3 (a: Int) (b: Int) (c: Int): Int = a
fn main (): Int = 1 2 3 add3.
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
                    let mainMethod =
                        hirModule.methods
                        |> List.tryFind (fun methodInfo -> symbolTable.Get(methodInfo.sym) |> Option.exists (fun symbolInfo -> symbolInfo.name = "main"))

                    match mainMethod with
                    | Some methodInfo ->
                        match methodInfo.body with
                        | Hir.Expr.Call (Hir.Callable.Fn _, _, [ Hir.Expr.Int (1, _); Hir.Expr.Int (2, _); Hir.Expr.Int (3, _) ], _, _) ->
                            Assert.True(true)
                        | other ->
                            Assert.True(false, $"expected multi-argument Hir.Expr.Call for dot-only syntax but got {other}")
                    | None ->
                        Assert.True(false, "main method was not found in analyzed HIR module")
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
    let ``semantic analysis handles zero argument dot-only call`` () =
        let program = """
fn ping (): Int = 1
fn main (): Int = ping.
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
                    let mainMethod =
                        hirModule.methods
                        |> List.tryFind (fun methodInfo -> symbolTable.Get(methodInfo.sym) |> Option.exists (fun symbolInfo -> symbolInfo.name = "main"))

                    match mainMethod with
                    | Some methodInfo ->
                        match methodInfo.body with
                        | Hir.Expr.Call (Hir.Callable.Fn _, _, [], _, _) ->
                            Assert.True(true)
                        | other ->
                            Assert.True(false, $"expected zero-arg Hir.Expr.Call for ping. but got {other}")
                    | None ->
                        Assert.True(false, "main method was not found in analyzed HIR module")
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
    let ``void call should not be used as value`` () =
        let span = Span.Empty
        let importDecl = Ast.Decl.Import([ "System"; "Console" ], span) :> Ast.Decl
        let retType = Ast.TypeExpr.Unit(span) :> Ast.TypeExpr
        let writeLineExpr = Ast.Expr.StaticAccess("Console", "WriteLine", span) :> Ast.Expr
        let helloArg = Ast.Expr.String("Hello, World!", span) :> Ast.Expr
        let callExpr = Ast.Expr.Apply(writeLineExpr, [ helloArg ], span) :> Ast.Expr
        let letStmt = Ast.Stmt.Let("x", callExpr, span) :> Ast.Stmt
        let valueStmt = Ast.Stmt.ExprStmt(Ast.Expr.Id("x", span) :> Ast.Expr, span) :> Ast.Stmt
        let body = Ast.Expr.Block([ letStmt; valueStmt ], span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, span) :> Ast.Decl
        let astModule = Ast.Module([ importDecl; fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true } ->
            Assert.True(false, "void 呼び出しの値文脈利用は失敗するべきです。")
        | { diagnostics = diagnostics } ->
            Assert.NotEmpty(diagnostics)

    [<Fact>]
    let ``analyzeExpr should lower lambda expression into Hir Lambda`` () =
        let span = Span.Empty
        let intType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let retType = Ast.TypeExpr.Arrow(intType, intType, span) :> Ast.TypeExpr
        let lambdaBody = Ast.Expr.Id("x", span) :> Ast.Expr
        let lambdaExpr = Ast.Expr.Lambda([ "x" ], lambdaBody, span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, lambdaExpr, span) :> Ast.Decl
        let astModule = Ast.Module([ fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            match hirModule.methods.Head.body with
            | Hir.Expr.Lambda (args, retTid, body, lambdaTid, _) ->
                Assert.Single(args) |> ignore
                Assert.Equal(TypeId.Int, args.Head.typ)
                Assert.Equal(TypeId.Int, retTid)
                Assert.Equal(TypeId.Fn([ TypeId.Int ], TypeId.Int), lambdaTid)
                match body with
                | Hir.Expr.Id (_, bodyTid, _) -> Assert.Equal(TypeId.Int, bodyTid)
                | other -> Assert.True(false, $"expected lambda body to be Id but got {other}")
            | other ->
                Assert.True(false, $"expected Hir.Expr.Lambda but got {other}")
        | { diagnostics = diagnostics } ->
            let message =
                diagnostics
                |> List.map (fun err -> err.toDisplayText())
                |> String.concat "; "
            Assert.True(false, $"semantic analysis failed: {message}")

    [<Fact>]
    let ``analyzeExpr should report duplicate lambda parameter names`` () =
        let span = Span.Empty
        let intType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let retType = Ast.TypeExpr.Arrow(intType, intType, span) :> Ast.TypeExpr
        let lambdaBody = Ast.Expr.Id("x", span) :> Ast.Expr
        let lambdaExpr = Ast.Expr.Lambda([ "x"; "x" ], lambdaBody, span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, lambdaExpr, span) :> Ast.Decl
        let astModule = Ast.Module([ fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        let diagnostics =
            match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
            | { succeeded = true; value = Some hirModule } ->
                hirModule.methods.Head.getDiagnostics
            | { diagnostics = diagnostics } -> diagnostics

        Assert.Contains(diagnostics, fun d -> d.message.Contains("Duplicate lambda parameter 'x'"))

    [<Fact>]
    let ``analyzeExpr should preserve mutable variable capture in lambda body`` () =
        let program = """
fn main: () =
    do
        var x = 1
        let f = fn _ -> x
        ()
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
                    let hasMutableCaptureLambda =
                        hirModule.methods
                        |> List.exists (fun methodInfo ->
                            match methodInfo.body with
                            | Hir.Expr.Block (stmts, _, _, _) ->
                                stmts
                                |> List.exists (fun stmt ->
                                    match stmt with
                                    | Hir.Stmt.Let (_, _, Hir.Expr.Lambda (_, _, Hir.Expr.Id (_, capturedTid, _), _, _), _) ->
                                        capturedTid = TypeId.Int
                                    | _ -> false)
                            | _ -> false)
                    Assert.True(hasMutableCaptureLambda, "expected lambda capturing mutable int symbol in block")
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"semantic analysis failed: {message}")
            | Failure (reason, failureSpan) ->
                Assert.True(false, $"parse failed: {reason} at {failureSpan.left.Line}:{failureSpan.left.Column}")
        | Failure (reason, failureSpan) ->
            Assert.True(false, $"tokenize failed: {reason} at {failureSpan.left.Line}:{failureSpan.left.Column}")

    [<Fact>]
    let ``analyzeExpr should allow lambda capture from for loop scope`` () =
        let program = """
import System'Linq'Enumerable

fn main: () =
    do
        for i in 0 1 Enumerable'Range.
            let f = fn _ -> i
            ()
        ()
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
                    let hasLoopCaptureLambda =
                        hirModule.methods
                        |> List.exists (fun methodInfo ->
                            match methodInfo.body with
                            | Hir.Expr.Block (stmts, _, _, _) ->
                                stmts
                                |> List.exists (fun stmt ->
                                    match stmt with
                                    | Hir.Stmt.For (_, _, _, bodyStmts, _) ->
                                        bodyStmts
                                        |> List.exists (fun bodyStmt ->
                                            match bodyStmt with
                                            | Hir.Stmt.Let (_, _, Hir.Expr.Lambda (_, _, Hir.Expr.Id _, _, _), _) -> true
                                            | _ -> false)
                                    | _ -> false)
                            | _ -> false)
                    Assert.True(hasLoopCaptureLambda, "lambda capturing for-loop variable should be analyzed into HIR")
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"semantic analysis failed: {message}")
            | Failure (reason, failureSpan) ->
                Assert.True(false, $"parse failed: {reason} at {failureSpan.left.Line}:{failureSpan.left.Column}")
        | Failure (reason, failureSpan) ->
            Assert.True(false, $"tokenize failed: {reason} at {failureSpan.left.Line}:{failureSpan.left.Column}")

    [<Fact>]
    let ``void call should not be passed as argument value`` () =
        let span = Span.Empty
        let importConsole = Ast.Decl.Import([ "System"; "Console" ], span) :> Ast.Decl
        let importInt32 = Ast.Decl.Import([ "System"; "Int32" ], span) :> Ast.Decl
        let retType = Ast.TypeExpr.Unit(span) :> Ast.TypeExpr

        let writeLineExpr = Ast.Expr.StaticAccess("Console", "WriteLine", span) :> Ast.Expr
        let writeLineCall = Ast.Expr.Apply(writeLineExpr, [ Ast.Expr.String("x", span) :> Ast.Expr ], span) :> Ast.Expr
        let parseExpr = Ast.Expr.StaticAccess("Int32", "Parse", span) :> Ast.Expr
        let parseCall = Ast.Expr.Apply(parseExpr, [ writeLineCall ], span) :> Ast.Expr

        let bodyStmt = Ast.Stmt.ExprStmt(parseCall, span) :> Ast.Stmt
        let body = Ast.Expr.Block([ bodyStmt ], span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, span) :> Ast.Decl
        let astModule = Ast.Module([ importConsole; importInt32; fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true } ->
            Assert.True(false, "void 呼び出しを引数値として渡す場合は失敗するべきです。")
        | { diagnostics = diagnostics } ->
            Assert.NotEmpty(diagnostics)

    [<Fact>]
    let ``void call should be allowed in expression statement`` () =
        let span = Span.Empty
        let importDecl = Ast.Decl.Import([ "System"; "Console" ], span) :> Ast.Decl
        let retType = Ast.TypeExpr.Unit(span) :> Ast.TypeExpr
        let writeLineExpr = Ast.Expr.StaticAccess("Console", "WriteLine", span) :> Ast.Expr
        let callExpr = Ast.Expr.Apply(writeLineExpr, [ Ast.Expr.String("ok", span) :> Ast.Expr ], span) :> Ast.Expr
        let bodyStmt = Ast.Stmt.ExprStmt(callExpr, span) :> Ast.Stmt
        let body = Ast.Expr.Block([ bodyStmt ], span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, span) :> Ast.Decl
        let astModule = Ast.Module([ importDecl; fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            let hasError =
                hirModule.methods
                |> List.exists (fun m -> m.hasError)
            Assert.False(hasError, "式文コンテキストでの void 呼び出しは許可されるべきです。")
        | { diagnostics = diagnostics } ->
            let message =
                diagnostics
                |> List.map (fun err -> err.toDisplayText())
                |> String.concat "; "
            Assert.True(false, $"semantic analysis failed unexpectedly: {message}")

    [<Fact>]
    let ``non-callable expression diagnostic should include reason`` () =
        let span = Span.Empty
        let retType = Ast.TypeExpr.Unit(span) :> Ast.TypeExpr
        let callExpr = Ast.Expr.Apply(Ast.Expr.Unit(span) :> Ast.Expr, [ Ast.Expr.Unit(span) :> Ast.Expr ], span) :> Ast.Expr
        let callStmt = Ast.Stmt.ExprStmt(callExpr, span) :> Ast.Stmt
        let body = Ast.Expr.Block([ callStmt ], span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, span) :> Ast.Decl
        let astModule = Ast.Module([ fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        let diagnosticMessages =
            match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
            | { succeeded = true; value = Some hirModule } ->
                let mainSid = hirModule.scope.vars.["main"]
                let mainMethod = hirModule.methods |> List.find (fun methodInfo -> methodInfo.sym.id = mainSid.id)
                mainMethod.getDiagnostics |> List.map (fun d -> d.message)
            | { diagnostics = diagnostics } ->
                diagnostics |> List.map (fun d -> d.message)

        // Calling a unit literal as a function should generate a diagnostic.
        Assert.True(diagnosticMessages.Length > 0, "No diagnostic was generated when calling a unit literal as a function.")
        // Verify that internal compiler representations ("target kind=") do not leak into diagnostics.
        Assert.DoesNotContain(diagnosticMessages, fun msg -> msg.Contains("target kind="))

    [<Fact>]
    let ``nullary function call with dot-only syntax should be allowed`` () =
        let program = """
import System'Console

fn greet (): () = "hello!" Console'WriteLine.

fn main: () = greet.
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "`greet.` の0引数呼び出し解析に失敗しています。")
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
    let ``unit argument call should not be normalized to nullary call`` () =
        let program = """
import System'Console

fn greet (): () = "hello!" Console'WriteLine.

fn main: () = () greet.
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
                | { succeeded = true } ->
                    Assert.True(false, "`() greet.` は 0 引数関数呼び出しとして成功してはいけません。")
                | { diagnostics = diagnostics } ->
                    let messages = diagnostics |> List.map (fun d -> d.message)
                    Assert.Contains(messages, fun msg -> msg.Contains("different number of arguments"))
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``for statement accepts Enumerable.Range enumerable`` () =
        let program = """
import System'Console

import System'Linq'Enumerable

fn main: () = do
    for i in 1 20 Enumerable'Range.
        i Console'WriteLine.
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "Enumerable.Range による for 解析に失敗しています。")
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
    let ``string split with implicit optional argument should be analyzed`` () =
        let program = """
import System'Int32
import System'Console

fn main: () = do
    var n_x = Console'ReadLine.
    n_x = " " n_x'Split.
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "`Split \" \"` の解析に失敗しています。")
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
    let ``instance member call should work for imported named type arguments`` () =
        let program = """
import System'Text'StringBuilder

fn appendExclamation (sb: StringBuilder): () = do
    let ignored = "!" sb'Append.
    ()
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "import された型（TypeId.Name）を引数に持つインスタンスメソッド呼び出し解析に失敗しています。")
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
    let ``array index access should be analyzed`` () =
        let program = """
import System'Console

fn main: () = do
    let line = Console'ReadLine.
    let a = " " line'Split.
    a !! 0 Console'WriteLine.
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "`a !! 0` の解析に失敗しています。")
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
    let ``array String type application should be analyzed`` () =
        let program = "fn join (xs: Array String): () = ()"
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
                    let joinMethod =
                        hirModule.methods
                        |> List.tryFind (fun meth ->
                            match hirModule.scope.vars.TryGetValue("join") with
                            | true, sid -> sid.id = meth.sym.id
                            | _ -> false)

                    match joinMethod with
                    | Some methodInfo ->
                        match Type.resolve subst methodInfo.typ with
                        | TypeId.Fn([TypeId.App(TypeId.Native arrayType, [ TypeId.String ])], TypeId.Unit)
                            when arrayType = typeof<System.Array> -> Assert.True(true)
                        | other -> Assert.True(false, $"Unexpected method type: {other}")
                    | None ->
                        Assert.True(false, "join method was not found in HIR module")
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
    let ``array type application with too many type arguments should report diagnostic`` () =
        let program = "fn bad (xs: Array String Int): () = ()"
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
                    let badMethod =
                        hirModule.methods
                        |> List.tryFind (fun meth -> meth.sym.id = hirModule.scope.vars.["bad"].id)

                    match badMethod with
                    | Some methodInfo ->
                        match Type.resolve subst methodInfo.typ with
                        | TypeId.Fn ([ TypeId.Error message ], TypeId.Unit)
                        | TypeId.Fn ([ TypeId.Error message ], TypeId.Error _) ->
                            Assert.Contains("Array type expects exactly one type argument", message)
                        | other ->
                            Assert.True(false, $"Unexpected method type: {other}")
                    | None ->
                        Assert.True(false, "bad method was not found")
                | { diagnostics = diagnostics } ->
                    let message =
                        diagnostics
                        |> List.map (fun err -> err.toDisplayText())
                        |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed unexpectedly: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``non-array type application should be preserved as TypeId.App`` () =
        let program = "fn bad (xs: String Int): () = ()"
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
                    let badMethod =
                        hirModule.methods
                        |> List.tryFind (fun meth -> meth.sym.id = hirModule.scope.vars.["bad"].id)

                    match badMethod with
                    | Some methodInfo ->
                        match Type.resolve subst methodInfo.typ with
                        | TypeId.Fn ([ TypeId.App (TypeId.String, [ TypeId.Int ]) ], TypeId.Unit) ->
                            Assert.True(true)
                        | other ->
                            Assert.True(false, $"Unexpected method type: {other}")
                    | None ->
                        Assert.True(false, "bad method was not found")
                | { diagnostics = diagnostics } ->
                    let message =
                        diagnostics
                        |> List.map (fun err -> err.toDisplayText())
                        |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed unexpectedly: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``for range with array length and index access should be analyzed`` () =
        let program = """
import System'Int32
import System'Console
import System'Linq'Enumerable

fn main: () = do
    let line = Console'ReadLine.
    let a = " " line'Split.
    for i in 0 a'Length Enumerable'Range.
        a !! i Console'WriteLine.
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "`Enumerable.Range 0 a.Length` + `a !! i` の解析に失敗しています。")
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
    let ``generic apply on static method should be analyzed`` () =
        let program = """
import System'Activator

fn main (): Int = Activator'CreateInstance[Int].
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "`Activator.CreateInstance[Int] ()` の解析に失敗しています。")
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
    let ``imported constructor call should be analyzed`` () =
        let program = """
import System'Text'StringBuilder

fn createBuilder (): StringBuilder = StringBuilder.
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "`StringBuilder ()` のコンストラクタ解決に失敗しています。")
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
    let ``member access assignment with writable property should be analyzed`` () =
        let program = """
import System'Console

fn main (): () = do
    Console'WindowWidth = 120
    ()
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "`Console'WindowWidth = 120` の解析に失敗しています。")
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
    let ``member access assignment reports read only property`` () =
        let program = """
import System'DateTime

fn main (): () = do
    DateTime'Now = DateTime'Now
    ()
"""

        let input: Input<SourceChar> = StringInput program

        let diagnostics =
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
                        hirModule.methods |> List.collect (fun methodInfo -> methodInfo.getDiagnostics)
                    | { diagnostics = diagnostics } -> diagnostics
                | Failure (reason, span) ->
                    [ Diagnostic.Error($"Parsing failed: {reason}", span) ]
            | Failure (reason, span) ->
                [ Diagnostic.Error($"Lexing failed: {reason}", span) ]

        Assert.Contains(diagnostics, fun diagnostic -> diagnostic.message.Contains("read-only"))

    [<Fact>]
    let ``extension method call should be analyzed`` () =
        let program = """
import Atla'Core'Tests'Semantics'TestExtensions

fn addTen (): () = do
    let x = 1
    x'PlusTen.
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "`x.PlusTen ()` の拡張メソッド解決に失敗しています。")
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

    // ─────────────────────────────────────────────────────────────
    // 第一級関数テスト
    // ─────────────────────────────────────────────────────────────

    /// パーサが `Int -> Int` を Ast.TypeExpr.Arrow としてパースできることを検証する。
    [<Fact>]
    let ``parser should parse arrow type expression`` () =
        let program = """
fn apply (f: Int -> Int) (x: Int): Int = x
"""
        let input: Input<SourceChar> = StringInput program
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Success (moduleAst, _) ->
                let decl = moduleAst.decls.Head :?> Ast.Decl.Fn
                let firstArg = decl.args.Head :?> Ast.FnArg.Named
                Assert.IsType<Ast.TypeExpr.Arrow>(firstArg.typeExpr) |> ignore
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    /// 関数型引数を持つ atla 関数の意味解析が成功することを検証する。
    [<Fact>]
    let ``semantic analysis supports higher-order function parameters`` () =
        let program = """
fn twice (x: Int): Int = x + x
fn apply (f: Int -> Int) (x: Int): Int = x f.
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "高階関数の意味解析でエラーが発生しました。")
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

    /// 関数型引数を持つメソッドのレイアウトが正常に完了することを検証する。
    [<Fact>]
    let ``layout supports higher-order function parameters`` () =
        let program = """
fn twice (x: Int): Int = x + x
fn apply (f: Int -> Int) (x: Int): Int = x f.
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
                    let hirAssembly = Hir.Assembly("TestAsm", [ hirModule ])
                    match ClosureConversion.preprocessAssembly(symbolTable, hirAssembly) with
                    | { succeeded = true; value = Some closedAsm } ->
                        match Layout.layoutAssembly("TestAsm", closedAsm) with
                        | { succeeded = true; value = Some mirAsm } ->
                            let methods = mirAsm.modules.Head.methods
                            Assert.Equal(2, methods.Length)
                        | { diagnostics = diagnostics } ->
                            let message =
                                diagnostics
                                |> List.map (fun err -> err.toDisplayText())
                                |> String.concat "; "
                            Assert.True(false, $"Layout failed: {message}")
                    | { diagnostics = diagnostics } ->
                        let message =
                            diagnostics
                            |> List.map (fun err -> err.toDisplayText())
                            |> String.concat "; "
                        Assert.True(false, $"Closure conversion failed: {message}")
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

    /// atla 関数を .NET メソッドへデリゲートとして渡す意味解析が成功することを検証する。
    /// Fn([Int], Int) が Converter<int,int> などのデリゲート型と統一できることを検証する。
    [<Fact>]
    let ``semantic analysis supports passing atla function to .NET delegate parameter`` () =
        // TypeId.Fn が .NET デリゲート型と canUnify できることを確認する。
        let subst = TypeSubst()
        let canUnify a b = Type.canUnify subst a b
        Assert.True(canUnify (TypeId.Fn([ TypeId.Int ], TypeId.Int)) (TypeId.Native typeof<System.Converter<int, int>>), "Fn([Int], Int) は Converter<int,int> と統一できなければならない。")
        Assert.True(canUnify (TypeId.Fn([ TypeId.Int ], TypeId.Int)) (TypeId.Native typeof<System.Func<int, int>>), "Fn([Int], Int) は Func<int,int> と統一できなければならない。")
        Assert.True(canUnify (TypeId.Fn([ TypeId.Int ], TypeId.Unit)) (TypeId.Native typeof<System.Action<int>>), "Fn([Int], Unit) は Action<int> と統一できなければならない。")
        Assert.False(canUnify (TypeId.Fn([ TypeId.Int ], TypeId.Int)) (TypeId.Native typeof<System.Action<int>>), "Fn([Int], Int) は戻り値の型が異なるため Action<int> と統一できてはならない。")

    /// `Hir.Method.args` が宣言順で引数を保持することを検証する。
    [<Fact>]
    let ``Hir.Method.args preserves parameter declaration order`` () =
        let program = """
fn apply (f: Int -> Int) (x: Int): Int = x f.
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
                    let applyMethod = hirModule.methods.Head
                    // apply は (f: Int -> Int, x: Int) の順で 2 引数を持つ。
                    Assert.Equal(2, applyMethod.args.Length)
                    let (_, firstArgType) = applyMethod.args.[0]
                    let (_, secondArgType) = applyMethod.args.[1]
                    Assert.Equal(TypeId.Fn([ TypeId.Int ], TypeId.Int), Type.resolve subst firstArgType)
                    Assert.Equal(TypeId.Int, Type.resolve subst secondArgType)
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

    // ─────────────────────────────────────────────────────────────
    // エラーメッセージ・エラー伝播テスト
    // ─────────────────────────────────────────────────────────────

    /// 型不一致エラーのメッセージが TypeId の F# 内部表現ではなく
    /// 人間が読みやすい型名（"int", "string" 等）を含むことを検証する。
    [<Fact>]
    let ``unify error message uses human readable type names`` () =
        let span = Span.Empty
        // fn bad (): Int = "hello"  → Int と String が不一致
        let retTypeExpr = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let bodyExpr = Ast.Expr.String("hello", span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("bad", [], retTypeExpr, bodyExpr, span) :> Ast.Decl
        let astModule = Ast.Module([ fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = false; diagnostics = diagnostics } ->
            Assert.NotEmpty(diagnostics)
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            // F# 内部表現（"Int" や "String"）ではなく、短い型名が含まれるべき。
            Assert.Contains("int", message)
            Assert.Contains("string", message)
            Assert.DoesNotContain("TypeId", message)
        | { succeeded = true } ->
            Assert.True(false, "型不一致のプログラムが成功してはならない。")

    /// do ブロックの末尾式が ExprError のとき、余分な "Cannot unify" エラーが
    /// 報告されず、根本エラーのみが診断に現れることを検証する。
    [<Fact>]
    let ``block with ExprError as last expression propagates root error without extra Cannot unify`` () =
        // fn bad (): Int = do
        //     undefinedVar
        // ここで undefinedVar は未定義 → ExprError が末尾式となる。
        // 従来はさらに "Cannot unify types: int and unknown" が報告されていたが、
        // 修正後は根本エラー（未定義変数）のみが報告されるべき。
        let program = """
fn bad (): Int = do
    undefinedVar
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
                    let messages = diagnostics |> List.map (fun d -> d.toDisplayText())
                    let combined = messages |> String.concat "; "
                    // 根本エラー（未定義変数）が報告されるべき。
                    Assert.Contains("undefinedVar", combined)
                    // "Cannot unify" は根本原因ではないため報告されてはならない。
                    Assert.DoesNotContain("Cannot unify", combined)
                | { succeeded = true } ->
                    Assert.True(false, "未定義変数を含むプログラムが成功してはならない。")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    // ─────────────────────────────────────────────────────────────
    // フェーズ境界テスト（Resolve / Infer）（Task 5: line 1038）
    // ─────────────────────────────────────────────────────────────

    /// Resolve フェーズ単独で import 宣言がモジュールスコープに登録されることを検証する。
    [<Fact>]
    let ``Resolve.resolveModule registers imported type in module scope`` () =
        let span = Span.Empty
        let importDecl = Ast.Decl.Import([ "System"; "Console" ], span) :> Ast.Decl
        let astModule = Ast.Module([ importDecl ])
        let symbolTable = SymbolTable()

        match Resolve.resolveModule(symbolTable, "main", astModule) with
        | { succeeded = true; value = Some resolved } ->
            // Console がスコープに登録されていることを確認する。
            let consoleTyp = resolved.moduleScope.ResolveType("Console")
            Assert.True(consoleTyp.IsSome, "Resolve フェーズで Console 型がスコープに登録されていない。")
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            Assert.True(false, $"Resolve.resolveModule failed: {message}")

    /// Resolve フェーズ単独で fn 宣言が収集されることを検証する。
    [<Fact>]
    let ``Resolve.resolveModule collects fn declarations`` () =
        let span = Span.Empty
        let retType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let fnDecl = Ast.Decl.Fn("main", [], retType, Ast.Expr.Int(0, span) :> Ast.Expr, span) :> Ast.Decl
        let astModule = Ast.Module([ fnDecl ])
        let symbolTable = SymbolTable()

        match Resolve.resolveModule(symbolTable, "main", astModule) with
        | { succeeded = true; value = Some resolved } ->
            Assert.Equal(1, resolved.fnDecls.Length)
            Assert.Equal("main", resolved.fnDecls.Head.name)
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            Assert.True(false, $"Resolve.resolveModule failed: {message}")

    /// Infer フェーズ単独でメタ型変数が具体型へ解決されることを検証する。
    [<Fact>]
    let ``Infer.inferModule resolves meta type variables to concrete types`` () =
        let span = Span.Empty
        let symbolTable = SymbolTable()
        let subst = TypeSubst()

        // HIR メソッドに meta 型変数を含めて、Infer が具体化することを確認する。
        let methodSym = symbolTable.NextId()
        let scope = Scope(None)
        scope.DeclareVar("id", methodSym)

        let argSid = symbolTable.NextId()
        // 引数型を Int に固定し、meta 経由で解決させる。
        let metaFactory = TypeMetaFactory()
        let metaTid = TypeId.Meta(metaFactory.Fresh())
        Type.unify subst metaTid TypeId.Int |> ignore

        let hirMethod = Hir.Method(methodSym, [ argSid, metaTid ], Hir.Expr.Id(argSid, metaTid, span), TypeId.Fn([ metaTid ], metaTid), span)
        let hirModule = Hir.Module("Main", [], [], [ hirMethod ], scope)

        match Infer.inferModule(subst, hirModule) with
        | Result.Ok inferredModule ->
            let inferredMethod = inferredModule.methods.Head
            let argType = inferredMethod.args |> List.head |> snd
            // meta 型変数が Int に解決されていること。
            Assert.Equal(TypeId.Int, argType)
        | Result.Error diagnostics ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            Assert.True(false, $"Infer.inferModule failed: {message}")

    /// Resolve と Infer を順番に組み合わせたフェーズ境界テスト。
    [<Fact>]
    let ``Resolve then Infer pipeline produces fully typed HIR for simple function`` () =
        let span = Span.Empty
        let retType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let fnDecl = Ast.Decl.Fn("answer", [], retType, Ast.Expr.Int(42, span) :> Ast.Expr, span) :> Ast.Decl
        let astModule = Ast.Module([ fnDecl ])
        let symbolTable = SymbolTable()
        let subst = TypeSubst()

        // フェーズ1: Resolve でシンボル解決・スコープ構築。
        match Resolve.resolveModule(symbolTable, "main", astModule) with
        | { succeeded = true; value = Some _ } ->
            // フェーズ2: Analyze 全体（Resolve + Analyze + Infer）で HIR 生成。
            match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
            | { succeeded = true; value = Some hirModule } ->
                let answerMethod = hirModule.methods |> List.tryFind (fun m -> m.sym.id = hirModule.scope.vars.["answer"].id)
                match answerMethod with
                | Some meth ->
                    // Infer フェーズ後の型が具体化されていることを確認する。
                    match Type.resolve subst meth.typ with
                    | TypeId.Fn ([], TypeId.Int) -> Assert.True(true)
                    | other -> Assert.True(false, $"Unexpected method type after Infer: {other}")
                | None -> Assert.True(false, "answer method not found in HIR")
            | { diagnostics = diagnostics } ->
                let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                Assert.True(false, $"analyzeModule failed: {message}")
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            Assert.True(false, $"Resolve.resolveModule failed: {message}")


    /// import ターゲットがモジュールと型の両方で存在する場合、モジュール解決を優先する。
    [<Fact>]
    let ``Resolve.resolveModule prefers module import when module and type share same path`` () =
        let span = Span.Empty
        let importDecl = Ast.Decl.Import([ "Foo"; "Bar" ], span) :> Ast.Decl
        let astModule = Ast.Module([ importDecl ])
        let symbolTable = SymbolTable()

        let availableModules = Set.ofList [ "Foo.Bar" ]
        let availableTypes = Set.ofList [ "Foo.Bar" ]

        match Resolve.resolveModuleWithImports(symbolTable, "main", astModule, availableModules, availableTypes) with
        | { succeeded = true; value = Some resolved } ->
            Assert.True(resolved.importedModules.ContainsKey("Bar"), "モジュール import が優先されていない。")
            let importedType = resolved.moduleScope.ResolveType("Bar")
            Assert.True(importedType.IsNone, "モジュール優先時に型 import を同時適用してはならない。")
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            Assert.True(false, $"Resolve.resolveModuleWithImports failed: {message}")

    /// モジュールが存在しない場合、availableTypeFullNames で指定された型 import にフォールバックする。
    [<Fact>]
    let ``Resolve.resolveModule falls back to type import when module does not exist`` () =
        let span = Span.Empty
        let importDecl = Ast.Decl.Import([ "Foo"; "Bar" ], span) :> Ast.Decl
        let astModule = Ast.Module([ importDecl ])
        let symbolTable = SymbolTable()

        let availableModules = Set.empty<string>
        let availableTypes = Set.ofList [ "Foo.Bar" ]

        match Resolve.resolveModuleWithImports(symbolTable, "main", astModule, availableModules, availableTypes) with
        | { succeeded = true; value = Some resolved } ->
            let importedType = resolved.moduleScope.ResolveType("Bar")
            Assert.True(importedType.IsNone, "Resolve フェーズでは型 alias 実体化を行わない。Analyze で実体化される。")
            Assert.True(resolved.importedTypeAliases.ContainsKey("Bar"), "型 import alias が収集されていない。")
            Assert.False(resolved.importedModules.ContainsKey("Bar"), "型 import で importedModules が登録されてはならない。")
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            Assert.True(false, $"Resolve.resolveModuleWithImports failed: {message}")

    /// ロードされていないアセンブリの型を import すると Warning 診断が出る。
    [<Fact>]
    let ``import of unresolvable type emits a Warning diagnostic`` () =
        // "Non.Existent.Type" はどのアセンブリにも存在しない型。
        let span = Span.Empty
        let importDecl = Ast.Decl.Import([ "Non"; "Existent"; "Type" ], span) :> Ast.Decl
        let retType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let fnDecl = Ast.Decl.Fn("main", [], retType, Ast.Expr.Int(0, span) :> Ast.Expr, span) :> Ast.Decl
        let astModule = Ast.Module([ importDecl; fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()

        // resolveModule は Warning 診断が存在しても succeeded = true を返す。
        let resolveResult = Resolve.resolveModule(symbolTable, "main", astModule)
        Assert.True(resolveResult.succeeded, "resolveModule should succeed even with unresolvable import")

        let warnings = resolveResult.diagnostics |> List.filter (fun d -> d.severity = DiagnosticSeverity.Warning)
        Assert.NotEmpty(warnings)

        let warnMessage = warnings |> List.head |> (fun d -> d.message)
        Assert.Contains("Non.Existent.Type", warnMessage)
        Assert.Contains("atla.yaml", warnMessage)

    /// ロードされていない型を値として使用した際に具体的なエラーメッセージが出る。
    [<Fact>]
    let ``using unresolvable imported type as value gives specific error message`` () =
        // "Non.Existent.MyClass" はどのアセンブリにも存在しない型。
        // ユーザーが MyClass() のように値として使おうとする状況を再現する。
        let program = """
import Non'Existent'MyClass

fn main (): Int = MyClass.
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
                let result = Analyze.analyzeModule(symbolTable, subst, "main", moduleAst)

                // エラー診断が存在する（型が未解決なため）。
                let errors = result.diagnostics |> List.filter (fun d -> d.isError)
                Assert.NotEmpty(errors)

                // エラーメッセージに atla.yaml への言及が含まれ、
                // "undefined variable" の代わりに具体的な案内が出ること。
                let errorMessages = errors |> List.map (fun d -> d.message) |> String.concat " "
                Assert.Contains("atla.yaml", errorMessages)
                Assert.DoesNotContain("Undefined variable 'MyClass'", errorMessages)
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    /// Float 同士の四則演算がビルトイン演算子解決で成功することを確認する。
    [<Fact>]
    let ``semantic analysis resolves float builtin arithmetic operators`` () =
        let program = """
fn main (): Float = 2.0 * 3.0 + 1.0
"""
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
            | Success (moduleAst, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", moduleAst) with
                | { succeeded = true; value = Some hirModule } ->
                    let mainMethod =
                        hirModule.methods
                        |> List.tryFind (fun methodInfo ->
                            symbolTable.Get(methodInfo.sym)
                            |> Option.exists (fun symbolInfo -> symbolInfo.name = "main"))

                    match mainMethod with
                    | Some methodInfo ->
                        match methodInfo.typ with
                        | TypeId.Fn([], TypeId.Float) -> Assert.True(true)
                        | other -> Assert.True(false, $"Expected main type to be () -> Float but got {other}")
                    | None ->
                        Assert.True(false, "main method was not found in analyzed HIR")
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")

    /// NativeMethodGroup のオーバーロードが実引数型で一意に解決されることを確認する。
    [<Fact>]
    let ``semantic analysis resolves Console WriteLine overload by argument type`` () =
        let program = """
import System'Console

fn main: () = do
    42 Console'WriteLine.
"""
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
            | Success (moduleAst, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", moduleAst) with
                | { succeeded = true } ->
                    Assert.True(true)
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")

    /// impl メソッド呼び出し結果（一時的に Meta を含み得る式）でも、WriteLine の overload を Apply 段階で解決できることを確認する。
    [<Fact>]
    let ``semantic analysis resolves Console WriteLine for impl-method result`` () =
        let program = """
import System'Console

data Line =
    { slope: Float
    , intercept: Float
    }

impl Line
    fn evaluate (this: Line) (x: Float): Float =
        x

fn main (): () = do
    let line = Line { slope = 2.0, intercept = -1.0 }
    5.0 line'evaluate. Console'WriteLine.
"""
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule() tokenInput start with
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
            | Success (moduleAst, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", moduleAst) with
                | { succeeded = true } ->
                    Assert.True(true)
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")

    [<Fact>]
    let ``analyzeModule returns partial hir when semantic diagnostics contain errors`` () =
        let span = Span.Empty
        let intType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let body = Ast.Expr.Id("unknownValue", span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], intType, body, span) :> Ast.Decl
        let astModule = Ast.Module([ fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        let result = Analyze.analyzeModule(symbolTable, subst, "main", astModule)

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun diagnostic -> diagnostic.isError))
        Assert.True(result.value.IsSome, "意味エラー時も IntelliSense 用の部分 HIR を返す必要があります。")
