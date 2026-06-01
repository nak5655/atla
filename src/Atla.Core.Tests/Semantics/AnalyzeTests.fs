namespace Atla.Core.Tests.Semantics

open Xunit
open Atla.Core.Data
open Atla.Core.Syntax
open Atla.Core.Syntax.Data
open Atla.Core.Semantics
open Atla.Core.Semantics.Data
open Atla.Core.Semantics.Data.AnalyzeEnv
open Atla.Core.Lowering

module AnalyzeTests =
    /// 式木を走査して、述語を満たすラムダ式が含まれているか判定する。
    let rec private exprContainsLambda (predicate: Hir.Expr -> bool) (expr: Hir.Expr) : bool =
        let rec stmtContainsLambda (stmt: Hir.Stmt) : bool =
            match stmt with
            | Hir.Stmt.Let (_, _, valueExpr, _)
            | Hir.Stmt.Assign (_, valueExpr, _)
            | Hir.Stmt.ExprStmt (valueExpr, _) -> exprContainsLambda predicate valueExpr
            | Hir.Stmt.StoreField (instanceExpr, _, _, valueExpr, _) ->
                exprContainsLambda predicate instanceExpr || exprContainsLambda predicate valueExpr
            | Hir.Stmt.For (_, _, iterableExpr, bodyStmts, _) ->
                exprContainsLambda predicate iterableExpr || (bodyStmts |> List.exists stmtContainsLambda)
            | Hir.Stmt.If (condExpr, thenStmts, elseStmts, _) ->
                exprContainsLambda predicate condExpr
                || (thenStmts |> List.exists stmtContainsLambda)
                || (elseStmts |> List.exists stmtContainsLambda)
            | _ -> false

        let isMatch = match expr with | Hir.Expr.Lambda _ -> predicate expr | _ -> false

        let fromNested =
            match expr with
            | Hir.Expr.Call (_, instanceExpr, argExprs, _, _) ->
                let hasInstance = instanceExpr |> Option.exists (exprContainsLambda predicate)
                hasInstance || (argExprs |> List.exists (exprContainsLambda predicate))
            | Hir.Expr.Block (stmts, bodyExpr, _, _) ->
                (stmts |> List.exists stmtContainsLambda) || exprContainsLambda predicate bodyExpr
            | Hir.Expr.If (condExpr, thenExpr, elseExpr, _, _) ->
                exprContainsLambda predicate condExpr
                || exprContainsLambda predicate thenExpr
                || exprContainsLambda predicate elseExpr
            | Hir.Expr.Lambda (_, _, bodyExpr, _, _) -> exprContainsLambda predicate bodyExpr
            | Hir.Expr.MemberAccess (_, instanceExpr, _, _) ->
                instanceExpr |> Option.exists (exprContainsLambda predicate)
            | _ -> false

        isMatch || fromNested

    [<Fact>]
    let ``analyzeMethod infers argument reference type`` () =
        let span = Span.Empty
        let argType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let retType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let arg = Ast.FnArg.Named("x", argType, span) :> Ast.FnArg
        let body = Ast.Expr.Id("x", span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("id", [arg], retType, body, false, false, span) :> Ast.Decl
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
        let importDecl = Ast.Decl.Import([ "System"; "Console" ], false, span) :> Ast.Decl
        let retType = Ast.TypeExpr.Unit(span) :> Ast.TypeExpr
        let writeLineExpr = Ast.Expr.StaticAccess("Console", "WriteLine", span) :> Ast.Expr
        let helloArg = Ast.Expr.String("Hello, World!", span) :> Ast.Expr
        let callExpr = Ast.Expr.Apply(writeLineExpr, [ helloArg ], span) :> Ast.Expr
        let bodyStmt = Ast.Stmt.ExprStmt(callExpr, span) :> Ast.Stmt
        let body = Ast.Expr.Block([ bodyStmt ], span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, false, false, span) :> Ast.Decl
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
fn main (): Int
    val value = 1
    value
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
    let ``semantic analysis lowers union match into block and if chain`` () =
        let source = """
union Color
    object Black: Color
    struct Rgb: Color
        val r: Int
        val g: Int
        val b: Int

fn red (color: Color): Int
    match color
    | Color'Black -> 0
    | Color'Rgb { r, .. } -> r
"""

        let input: Input<SourceChar> = StringInput source
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
                    let redMethod =
                        hirModule.methods
                        |> List.find (fun methodDef ->
                            match symbolTable.Get(methodDef.sym) with
                            | Some symInfo -> symInfo.name = "red"
                            | None -> false)
                    match redMethod.body with
                    | Hir.Expr.Block (_, Hir.Expr.Block ([ Hir.Stmt.Let _ ], Hir.Expr.If _, _, _), _, _) -> ()
                    | other -> Assert.True(false, $"Expected union match to lower into Hir.Expr.Block + Hir.Expr.If, got {other}")
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun diagnostic -> diagnostic.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed unexpectedly: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis lowers zero field union variant to data constructor call`` () =
        let source = """
union Color
    object Black: Color

fn main (): Color
    Color'Black
"""

        let input: Input<SourceChar> = StringInput source
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
                    match hirModule.methods.Head.body with
                    | Hir.Expr.Block (_, Hir.Expr.Call (Hir.Callable.DataConstructor (_, fieldSids), None, [], _, _), _, _) ->
                        Assert.Empty(fieldSids)
                    | other ->
                        Assert.True(false, $"Expected zero-field union variant to lower into DataConstructor call, got {other}")
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun diagnostic -> diagnostic.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed unexpectedly: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis reports non exhaustive enum-style union match`` () =
        let source = """
union Color
    object Black: Color
    object White: Color

fn main (color: Color): Int
    match color
    | Color'Black -> 0
"""

        let input: Input<SourceChar> = StringInput source
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
                    Assert.Contains(diagnostics, fun diagnostic -> diagnostic.message.Contains("Non-exhaustive match"))
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for non-exhaustive union match")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis reports duplicate union match arms`` () =
        let source = """
union Color
    object Black: Color

fn main (color: Color): Int
    match color
    | Color'Black -> 0
    | Color'Black -> 1
"""

        let input: Input<SourceChar> = StringInput source
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
                    Assert.Contains(diagnostics, fun diagnostic -> diagnostic.message.Contains("Duplicate match arm"))
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for duplicate enum match arms")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis reports non exhaustive union match`` () =
        let source = """
union Shape
    struct Sq: Shape
        val side: Int
    struct Ci: Shape
        val rad: Int

fn main (shape: Shape): Int
    match shape
    | Shape'Sq { side, .. } -> side
"""
        let input: Input<SourceChar> = StringInput source
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
                    Assert.Contains(diagnostics, fun diagnostic -> diagnostic.message.Contains("Non-exhaustive match"))
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for non-exhaustive union match")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis allows non exhaustive match on extendable union`` () =
        let source = """
extendable union Shape
    struct Sq: Shape
        val side: Int
    struct Ci: Shape
        val rad: Int

fn main (shape: Shape): Int
    match shape
    | Shape'Sq { side, .. } -> side
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (moduleAst, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", moduleAst) with
                | { diagnostics = diagnostics } ->
                    Assert.DoesNotContain(diagnostics, fun diagnostic -> diagnostic.message.Contains("Non-exhaustive match"))
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis rejects external variant on sealed union`` () =
        let source = """
union Sealed
    struct A: Sealed
        val x: Int

struct B: Sealed
    val y: Int
"""
        let input: Input<SourceChar> = StringInput source
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
                    Assert.Contains(diagnostics, fun diagnostic -> diagnostic.message.Contains("sealed union"))
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for external variant on sealed union")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis accepts external variants on extendable union`` () =
        let source = """
extendable union Color
    struct Rgb: Color
        val r: Int

struct Cmyk: Color
    val k: Int

fn pick (c: Color): Color
    c
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (moduleAst, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", moduleAst) with
                | { diagnostics = diagnostics } ->
                    Assert.DoesNotContain(diagnostics, fun diagnostic -> diagnostic.isError)
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    /// 回帰テスト: do ブロック末尾式の型が unit でない場合に
    /// "Cannot unify: unit and T" が誤報告されなかったことを検証する。
    /// DataInit（具体型を直接返す式）を末尾に置くパターン。
    [<Fact>]
    let ``do block with non-unit last expression returns correct type`` () =
        let program = """
struct Point
    val x: Int
    val y: Int
fn makePoint (): Point
    val xv = 1
    val yv = 2
    { x = xv, y = yv } Point.
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
                | { succeeded = true } -> ()
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
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, false, false, span) :> Ast.Decl
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
        let program = "fn main (): Int\n    value'"
        let input: Input<SourceChar> = StringInput program

        let diagnostics =
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
        let incDecl = Ast.Decl.Fn("inc", [ incArg ], intType, incBody, false, false, span) :> Ast.Decl
        let mainBody =
            Ast.Expr.Apply(
                Ast.Expr.Id("inc", span) :> Ast.Expr,
                [ Ast.Expr.Int(1, span) :> Ast.Expr ],
                span
            ) :> Ast.Expr
        let mainDecl = Ast.Decl.Fn("main", [], intType, mainBody, false, false, span) :> Ast.Decl
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
fn main (): ()
    "hello" Console'WriteLine.
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
                    let mainMethod =
                        hirModule.methods
                        |> List.tryFind (fun methodInfo -> symbolTable.Get(methodInfo.sym) |> Option.exists (fun symbolInfo -> symbolInfo.name = "main"))

                    match mainMethod with
                    | Some methodInfo ->
                        match methodInfo.body with
                        | Hir.Expr.Block ([], Hir.Expr.Call (Hir.Callable.NativeMethod _, _, [ Hir.Expr.String ("hello", _) ], _, _), _, _) ->
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
struct Person
    val name: String
    val age: Int
fn buildPerson (): Person
    { name = "Alice", age = 20 } Person.
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
    let ``semantic analysis accepts impl method with self receiver and data member access`` () =
        let span = Span.Empty
        let doubleType = Ast.TypeExpr.Id("Double", span) :> Ast.TypeExpr
        let dataDecl =
            Ast.Decl.Data(
                "Line",
                [],
                [ Ast.DataItem.Field("slope", doubleType, false, span) :> Ast.DataItem
                  Ast.DataItem.Field("intercept", doubleType, false, span) :> Ast.DataItem ],
                span) :> Ast.Decl

        let thisArg = Ast.FnArg.Inferred("self", span) :> Ast.FnArg
        let xArg = Ast.FnArg.Named("x", doubleType, span) :> Ast.FnArg
        let evalBody = Ast.Expr.MemberAccess(Ast.Expr.Id("self", span) :> Ast.Expr, "slope", span) :> Ast.Expr
        let evalFn = Ast.Decl.Fn("evaluate", [ thisArg; xArg ], doubleType, evalBody, false, false, span)
        let implDecl = Ast.Decl.Impl("Line", [], None, None, None, [ evalFn ], span) :> Ast.Decl

        let lineInit =
            let recordLit =
                Ast.Expr.RecordLit(
                    [ Ast.DataInitField.Field("slope", Ast.Expr.Double(2.0, span) :> Ast.Expr, span) :> Ast.DataInitField
                      Ast.DataInitField.Field("intercept", Ast.Expr.Double(-1.0, span) :> Ast.Expr, span) :> Ast.DataInitField ],
                    span) :> Ast.Expr
            Ast.Expr.Apply(Ast.Expr.Id("Line", span) :> Ast.Expr, [ recordLit ], span) :> Ast.Expr
        let callBody =
            Ast.Expr.Apply(
                Ast.Expr.MemberAccess(lineInit, "evaluate", span) :> Ast.Expr,
                [ Ast.Expr.Double(5.0, span) :> Ast.Expr ],
                span) :> Ast.Expr
        let mainDecl = Ast.Decl.Fn("main", [], doubleType, callBody, false, false, span) :> Ast.Decl

        let astModule = Ast.Module([ dataDecl; implDecl; mainDecl ])
        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            // インスタンス impl メソッドは hirType.methods へルーティングされる。
            let allMethods =
                hirModule.methods
                @ (hirModule.types |> List.filter (fun t -> not t.isInterface) |> List.collect (fun t -> t.methods))
            let hasEvaluate =
                allMethods
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
                [],
                [ Ast.DataItem.Field("value", intType, false, span) :> Ast.DataItem ],
                span) :> Ast.Decl

        let oneFn = Ast.Decl.Fn("one", [], intType, Ast.Expr.Int(1, span) :> Ast.Expr, false, false, span)
        let implDecl = Ast.Decl.Impl("Line", [], None, None, None, [ oneFn ], span) :> Ast.Decl
        let staticCall = Ast.Expr.Apply(Ast.Expr.MemberAccess(Ast.Expr.Id("Line", span) :> Ast.Expr, "one", span) :> Ast.Expr, [], span) :> Ast.Expr
        let mainDecl = Ast.Decl.Fn("main", [], intType, staticCall, false, false, span) :> Ast.Decl

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
    let ``semantic analysis resolves static impl method as bare name within the same module`` () =
        // Regression test: static impl methods must be callable as bare names (e.g. `addButton`)
        // from other methods in the same module. Previously, only `Type'method` qualified syntax worked.
        let span = Span.Empty
        let intType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let dataDecl =
            Ast.Decl.Data(
                "Line",
                [],
                [ Ast.DataItem.Field("value", intType, false, span) :> Ast.DataItem ],
                span) :> Ast.Decl

        let helperFn = Ast.Decl.Fn("helper", [], intType, Ast.Expr.Int(42, span) :> Ast.Expr, false, false, span)
        let implDecl = Ast.Decl.Impl("Line", [], None, None, None, [ helperFn ], span) :> Ast.Decl

        // main calls `helper` as a bare name — this was previously "Undefined variable 'helper'"
        let bareNameCall = Ast.Expr.Apply(Ast.Expr.Id("helper", span) :> Ast.Expr, [], span) :> Ast.Expr
        let mainDecl = Ast.Decl.Fn("main", [], intType, bareNameCall, false, false, span) :> Ast.Decl

        let astModule = Ast.Module([ dataDecl; implDecl; mainDecl ])
        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true } ->
            Assert.True(true)
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            Assert.True(false, $"Semantic analysis failed unexpectedly: {message}")

    [<Fact>]
    let ``semantic analysis reports instance access to static impl method`` () =
        let span = Span.Empty
        let intType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let dataDecl =
            Ast.Decl.Data(
                "Line",
                [],
                [ Ast.DataItem.Field("value", intType, false, span) :> Ast.DataItem ],
                span) :> Ast.Decl

        let oneFn = Ast.Decl.Fn("one", [], intType, Ast.Expr.Int(1, span) :> Ast.Expr, false, false, span)
        let implDecl = Ast.Decl.Impl("Line", [], None, None, None, [ oneFn ], span) :> Ast.Decl
        let lineInit =
            let recordLit =
                Ast.Expr.RecordLit(
                    [ Ast.DataInitField.Field("value", Ast.Expr.Int(42, span) :> Ast.Expr, span) :> Ast.DataInitField ],
                    span) :> Ast.Expr
            Ast.Expr.Apply(Ast.Expr.Id("Line", span) :> Ast.Expr, [ recordLit ], span) :> Ast.Expr
        let badCall = Ast.Expr.Apply(Ast.Expr.MemberAccess(lineInit, "one", span) :> Ast.Expr, [], span) :> Ast.Expr
        let mainDecl = Ast.Decl.Fn("main", [], intType, badCall, false, false, span) :> Ast.Decl

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
struct A
    val value: Int
struct B
    val value: Int
impl A for B
    fn asInt self: Int
        self'value
impl B for A
    fn asInt self: Int
        self'value
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
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
    let ``semantic analysis resolves delegated native interface member via impl for by`` () =
        let source = """
import System'IDisposable
struct Box
    val items: IDisposable
impl IDisposable for Box by items
fn close (b: Box): ()
    b'Dispose.
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    let closeMethod =
                        hirModule.methods
                        |> List.tryFind (fun methodDef ->
                            match symbolTable.Get(methodDef.sym) with
                            | Some symInfo -> symInfo.name = "close"
                            | None -> false)
                    Assert.True(closeMethod.IsSome, "Expected delegated-member consumer method 'close' to be analyzed successfully")
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
    let ``semantic analysis reports error when impl for uses native class`` () =
        let source = """
import System'DateTime
struct Clock
    val dt: DateTime
impl DateTime for Clock by dt
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = false; diagnostics = diagnostics } ->
                    let hasExpectedDiagnostic =
                        diagnostics
                        |> List.exists (fun diagnostic -> diagnostic.message.Contains("must be a .NET interface type"))
                    Assert.True(hasExpectedDiagnostic, "Expected class-rejection diagnostic for impl for was not reported")
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for impl for with native class")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis allows multiple impl for blocks when roles differ`` () =
        let source = """
struct Shape
    val value: Int
struct Reader
    val marker: Int
struct Writer
    val marker: Int
impl Shape for Reader
    fn read self: Int
        self'value
impl Shape for Writer
    fn write self: Int
        self'value
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
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
struct Line
    val value: Int
impl Line
    fn first self: Int
        self'value
impl Line
    fn second self: Int
        self'value
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
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
struct Line
    val value: Int
struct Reader
    val marker: Int
impl Line for Reader
    fn first self: Int
        self'value
impl Line for Reader
    fn second self: Int
        self'value
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
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
    let ``semantic analysis reports error for impl as with interface type`` () =
        // IDisposable はインターフェイスなので impl ... as IDisposable は禁止。
        let source = """
import System'IDisposable
struct Resource
    val id: Int
impl Resource as IDisposable
    fn dispose self: Unit
        ()
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = false; diagnostics = diagnostics } ->
                    let hasExpectedDiagnostic =
                        diagnostics
                        |> List.exists (fun diagnostic -> diagnostic.message.Contains("cannot inherit from interface type"))
                    Assert.True(hasExpectedDiagnostic, "Expected 'cannot inherit from interface type' diagnostic was not reported")
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for impl as interface")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis reports error for impl as with sealed class`` () =
        // System.Math はシールドクラスなので impl ... as Math は禁止。
        let source = """
import System'Math
struct Token
    val value: Int
impl Token as Math
    fn run self: Unit
        ()
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = false; diagnostics = diagnostics } ->
                    let hasExpectedDiagnostic =
                        diagnostics
                        |> List.exists (fun diagnostic -> diagnostic.message.Contains("cannot inherit from sealed class"))
                    Assert.True(hasExpectedDiagnostic, "Expected 'cannot inherit from sealed class' diagnostic was not reported")
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for impl as sealed class")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis reports error for impl as with unimported type`` () =
        // import なしで as 句を使おうとした場合のエラー。
        let source = """
struct Widget
    val id: Int
impl Widget as UnknownBase
    fn run self: Unit
        ()
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = false; diagnostics = diagnostics } ->
                    let hasExpectedDiagnostic =
                        diagnostics
                        |> List.exists (fun diagnostic ->
                            diagnostic.message.Contains("UnknownBase") &&
                            (diagnostic.message.Contains("not defined") || diagnostic.message.Contains("not an imported")))
                    Assert.True(hasExpectedDiagnostic, "Expected error about undefined 'as' type was not reported")
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for impl as unimported type")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")



    [<Fact>]
    let ``semantic analysis handles chained dot-only calls`` () =
        let span = Span.Empty
        let intType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let mkArg name = Ast.FnArg.Named(name, intType, span) :> Ast.FnArg
        let idDecl = Ast.Decl.Fn("id", [ mkArg "x" ], intType, Ast.Expr.Id("x", span) :> Ast.Expr, false, false, span) :> Ast.Decl
        let incDecl = Ast.Decl.Fn("inc", [ mkArg "y" ], intType, Ast.Expr.Id("y", span) :> Ast.Expr, false, false, span) :> Ast.Decl

        let mainBody =
            Ast.Expr.Apply(
                Ast.Expr.Id("inc", span) :> Ast.Expr,
                [ Ast.Expr.Apply(Ast.Expr.Id("id", span) :> Ast.Expr, [ Ast.Expr.Int(1, span) :> Ast.Expr ], span) :> Ast.Expr ],
                span
            ) :> Ast.Expr
        let mainDecl = Ast.Decl.Fn("main", [], intType, mainBody, false, false, span) :> Ast.Decl
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
fn add3 (a: Int) (b: Int) (c: Int): Int
    a
fn main (): Int
    1 2 3 add3.
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
                    let mainMethod =
                        hirModule.methods
                        |> List.tryFind (fun methodInfo -> symbolTable.Get(methodInfo.sym) |> Option.exists (fun symbolInfo -> symbolInfo.name = "main"))

                    match mainMethod with
                    | Some methodInfo ->
                        match methodInfo.body with
                        | Hir.Expr.Block ([], Hir.Expr.Call (Hir.Callable.Fn _, _, [ Hir.Expr.Int (1, _); Hir.Expr.Int (2, _); Hir.Expr.Int (3, _) ], _, _), _, _) ->
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
fn ping (): Int
    1
fn main (): Int
    ping.
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
                    let mainMethod =
                        hirModule.methods
                        |> List.tryFind (fun methodInfo -> symbolTable.Get(methodInfo.sym) |> Option.exists (fun symbolInfo -> symbolInfo.name = "main"))

                    match mainMethod with
                    | Some methodInfo ->
                        match methodInfo.body with
                        | Hir.Expr.Block ([], Hir.Expr.Call (Hir.Callable.Fn _, _, [], _, _), _, _) ->
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
        let importDecl = Ast.Decl.Import([ "System"; "Console" ], false, span) :> Ast.Decl
        let retType = Ast.TypeExpr.Unit(span) :> Ast.TypeExpr
        let writeLineExpr = Ast.Expr.StaticAccess("Console", "WriteLine", span) :> Ast.Expr
        let helloArg = Ast.Expr.String("Hello, World!", span) :> Ast.Expr
        let callExpr = Ast.Expr.Apply(writeLineExpr, [ helloArg ], span) :> Ast.Expr
        let letStmt = Ast.Stmt.Let("x", None, callExpr, span) :> Ast.Stmt
        let valueStmt = Ast.Stmt.ExprStmt(Ast.Expr.Id("x", span) :> Ast.Expr, span) :> Ast.Stmt
        let body = Ast.Expr.Block([ letStmt; valueStmt ], span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, false, false, span) :> Ast.Decl
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
        let xArg = Ast.FnArg.Named("x", intType, span) :> Ast.FnArg
        let lambdaExpr = Ast.Expr.Lambda([ xArg ], lambdaBody, span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, lambdaExpr, false, false, span) :> Ast.Decl
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
        let xArg1 = Ast.FnArg.Named("x", intType, span) :> Ast.FnArg
        let xArg2 = Ast.FnArg.Named("x", intType, span) :> Ast.FnArg
        let lambdaExpr = Ast.Expr.Lambda([ xArg1; xArg2 ], lambdaBody, span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, lambdaExpr, false, false, span) :> Ast.Decl
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
    let ``resolveTypeExpr should resolve unit-arg arrow type () -> T as zero-arg Fn`` () =
        // () -> T の型アノテーションは fn () -> expr（0引数ラムダ）と一致するため、
        // TypeId.Fn([], T) に解決されるべきで、TypeId.Fn([Unit], T) であってはならない。
        let span = Span.Empty
        let unitType = Ast.TypeExpr.Unit(span) :> Ast.TypeExpr
        let intType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let retType = Ast.TypeExpr.Arrow(unitType, intType, span) :> Ast.TypeExpr
        let lambdaBody = Ast.Expr.Int(42, span) :> Ast.Expr
        let lambdaExpr = Ast.Expr.Lambda([], lambdaBody, span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, lambdaExpr, false, false, span) :> Ast.Decl
        let astModule = Ast.Module([ fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            match hirModule.methods.Head.body with
            | Hir.Expr.Lambda (args, retTid, _, lambdaTid, _) ->
                Assert.Empty(args)
                Assert.Equal(TypeId.Int, retTid)
                Assert.Equal(TypeId.Fn([], TypeId.Int), lambdaTid)
            | other ->
                Assert.True(false, $"expected Hir.Expr.Lambda but got {other}")
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
            Assert.True(false, $"semantic analysis failed: {message}")

    [<Fact>]
    let ``analyzeExpr should preserve mutable variable capture in lambda body`` () =
        let program = """
fn main: ()
    var x = 1
    val f = fn _ -> x
    ()
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
                    let hasMutableCaptureLambda =
                        hirModule.methods
                        |> List.exists (fun methodInfo ->
                            match methodInfo.body with
                            | Hir.Expr.Block (stmts, _, _, _) ->
                                stmts
                                |> List.exists (fun stmt ->
                                    match stmt with
                                    | Hir.Stmt.Let (_, _, valueExpr, _) ->
                                        exprContainsLambda
                                            (function
                                            | Hir.Expr.Lambda _ -> true
                                            | _ -> false)
                                            valueExpr
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

fn main: ()
    for i in 0 1 Enumerable'Range.
        val f = fn _ -> i
        ()
    ()
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
                                            | Hir.Stmt.Let (_, _, valueExpr, _) ->
                                                exprContainsLambda
                                                    (function
                                                    | Hir.Expr.Lambda _ -> true
                                                    | _ -> false)
                                                    valueExpr
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
        let importConsole = Ast.Decl.Import([ "System"; "Console" ], false, span) :> Ast.Decl
        let importInt32 = Ast.Decl.Import([ "System"; "Int32" ], false, span) :> Ast.Decl
        let retType = Ast.TypeExpr.Unit(span) :> Ast.TypeExpr

        let writeLineExpr = Ast.Expr.StaticAccess("Console", "WriteLine", span) :> Ast.Expr
        let writeLineCall = Ast.Expr.Apply(writeLineExpr, [ Ast.Expr.String("x", span) :> Ast.Expr ], span) :> Ast.Expr
        let parseExpr = Ast.Expr.StaticAccess("Int32", "Parse", span) :> Ast.Expr
        let parseCall = Ast.Expr.Apply(parseExpr, [ writeLineCall ], span) :> Ast.Expr

        let bodyStmt = Ast.Stmt.ExprStmt(parseCall, span) :> Ast.Stmt
        let body = Ast.Expr.Block([ bodyStmt ], span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, false, false, span) :> Ast.Decl
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
        let importDecl = Ast.Decl.Import([ "System"; "Console" ], false, span) :> Ast.Decl
        let retType = Ast.TypeExpr.Unit(span) :> Ast.TypeExpr
        let writeLineExpr = Ast.Expr.StaticAccess("Console", "WriteLine", span) :> Ast.Expr
        let callExpr = Ast.Expr.Apply(writeLineExpr, [ Ast.Expr.String("ok", span) :> Ast.Expr ], span) :> Ast.Expr
        let bodyStmt = Ast.Stmt.ExprStmt(callExpr, span) :> Ast.Stmt
        let body = Ast.Expr.Block([ bodyStmt ], span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, false, false, span) :> Ast.Decl
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
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, false, false, span) :> Ast.Decl
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

fn greet (): ()
    "hello!" Console'WriteLine.

fn main: ()
    greet.
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

fn greet (): ()
    "hello!" Console'WriteLine.

fn main: ()
    () greet.
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

fn main: ()
    for i in 1 20 Enumerable'Range.
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

fn main: ()
    var n_x = Console'ReadLine.
    n_x = " " n_x'Split.
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

fn appendExclamation (sb: StringBuilder): ()
    val ignored = "!" sb'Append.
    ()
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

fn main: ()
    val line = Console'ReadLine.
    val a = " " line'Split.
    a[0] Console'WriteLine.
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "`a[0]` の解析に失敗しています。")
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
        let program = "fn join (xs: Array String): ()\n    ()"
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
        let program = "fn bad (xs: Array String Int): ()\n    ()"
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
        let program = "fn bad (xs: String Int): ()\n    ()"
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

fn main: ()
    val line = Console'ReadLine.
    val a = " " line'Split.
    for i in 0 a'Length Enumerable'Range.
        a[i] Console'WriteLine.
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "`Enumerable.Range 0 a.Length` + `a[i]` の解析に失敗しています。")
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

fn main (): Int
    Activator'CreateInstance<Int>.
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "`Activator.CreateInstance<Int> ()` の解析に失敗しています。")
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

fn createBuilder (): StringBuilder
    StringBuilder.
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

    /// 回帰テスト: クラスとそのインターフェース双方に同名プロパティが存在する場合でも
    /// "Ambiguous member" が報告されないことを検証する。
    /// System.Collections.ArrayList.Count はクラス本体と ICollection / IList などの
    /// 複数インターフェース全てに Count プロパティが存在するためこのケースを確認できる。
    [<Fact>]
    let ``member access on property defined on both class and interface should not be ambiguous`` () =
        let program = """
import System'Collections'ArrayList

fn main (): ()
    val list = ArrayList.
    val count = list'Count
    ()
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "ArrayList'Count へのメンバーアクセスで Ambiguous member エラーが発生しました。")
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

fn main (): ()
    Console'WindowWidth = 120
    ()
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

fn main (): ()
    DateTime'Now = DateTime'Now
    ()
"""

        let input: Input<SourceChar> = StringInput program

        let diagnostics =
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

fn addTen (): ()
    val x = 1
    x'PlusTen.
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
fn apply (f: Int -> Int) (x: Int): Int
    x
"""
        let input: Input<SourceChar> = StringInput program
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
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
fn twice (x: Int): Int
    x + x
fn apply (f: Int -> Int) (x: Int): Int
    x f.
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
fn twice (x: Int): Int
    x + x
fn apply (f: Int -> Int) (x: Int): Int
    x f.
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

    /// Native 型同士の canUnify が .NET サブタイプ関係（IsAssignableFrom）を尊重することを検証する。
    /// InvalidOperationException は Exception を継承するため、Native(InvalidOperationException) は Native(Exception) と互換でなければならない。
    [<Fact>]
    let ``canUnify Native types respects .NET subtype inheritance`` () =
        let subst = TypeSubst()
        let canUnify a b = Type.canUnify subst a b
        // 等値は引き続き互換。
        Assert.True(canUnify (TypeId.Native typeof<System.InvalidOperationException>) (TypeId.Native typeof<System.InvalidOperationException>), "同じ型は自己と互換でなければならない。")
        // サブタイプは親型と互換（InvalidOperationException は Exception を継承する）。
        Assert.True(canUnify (TypeId.Native typeof<System.InvalidOperationException>) (TypeId.Native typeof<System.Exception>), "サブタイプ（InvalidOperationException）は親型（Exception）と互換でなければならない。")
        // 親型はサブタイプと互換ではない（逆方向は拒否）。
        Assert.False(canUnify (TypeId.Native typeof<System.Exception>) (TypeId.Native typeof<System.InvalidOperationException>), "親型（Exception）はサブタイプ（InvalidOperationException）と互換であってはならない。")
        // 無関係な型同士は互換ではない。
        Assert.False(canUnify (TypeId.Native typeof<System.InvalidOperationException>) (TypeId.Native typeof<System.IO.IOException>), "無関係な型同士は互換であってはならない。")

    /// サブタイプ引数を受け取るメソッド（ArrayList.Add）が正常に解決されることを検証する。
    /// System.Collections.ArrayList の Add は object 型を受け取るが、InvalidOperationException（サブタイプ）を渡せなければならない。
    [<Fact>]
    let ``Add method on collection resolves with subtype argument`` () =
        let program = """
import System'Collections'ArrayList
import System'InvalidOperationException

fn addSubtype: ()
    val list = ArrayList.
    val ex = InvalidOperationException.
    val _ = ex list'Add.
    ()
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "ArrayList.Add を InvalidOperationException（サブタイプ）引数で呼ぶ解析が失敗してはならない。")
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

    /// プロパティの型が System.Object であっても String 値を代入できることを検証する。
    /// Content プロパティ（obj 型）へ String を設定するケースで
    /// "Cannot unify types: System.Object and string" が出なくなることを確認する。
    [<Fact>]
    let ``property assignment of String to System.Object typed property succeeds`` () =
        let program = """
import System'Collections'ArrayList

fn test: ()
    val list = ArrayList.
    val s = "hello"
    val _ = s list'Add.
    ()
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "ArrayList.Add に String（System.Object のサブタイプ）を渡す解析が失敗してはならない。")
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

    /// `Hir.Method.args` が宣言順で引数を保持することを検証する。
    [<Fact>]
    let ``Hir.Method.args preserves parameter declaration order`` () =
        let program = """
fn apply (f: Int -> Int) (x: Int): Int
    x f.
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
        let fnDecl = Ast.Decl.Fn("bad", [], retTypeExpr, bodyExpr, false, false, span) :> Ast.Decl
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
fn bad (): Int
    undefinedVar
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
        let importDecl = Ast.Decl.Import([ "System"; "Console" ], false, span) :> Ast.Decl
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
        let fnDecl = Ast.Decl.Fn("main", [], retType, Ast.Expr.Int(0, span) :> Ast.Expr, false, false, span) :> Ast.Decl
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

        let hirMethod = Hir.Method(methodSym, [ argSid, metaTid ], Hir.Expr.Id(argSid, metaTid, span), TypeId.Fn([ metaTid ], metaTid), None, false, span)
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
        let fnDecl = Ast.Decl.Fn("answer", [], retType, Ast.Expr.Int(42, span) :> Ast.Expr, false, false, span) :> Ast.Decl
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


    /// import ターゲットが同一優先順位層でモジュールと型の両方に存在する場合、曖昧エラーとする。
    [<Fact>]
    let ``Resolve.resolveModule reports ambiguity when module and type share same source path`` () =
        let span = Span.Empty
        let importDecl = Ast.Decl.Import([ "Foo"; "Bar" ], false, span) :> Ast.Decl
        let astModule = Ast.Module([ importDecl ])
        let symbolTable = SymbolTable()

        let availableModules = Set.ofList [ "Foo.Bar" ]
        let availableTypes = Set.ofList [ "Foo.Bar" ]

        match Resolve.resolveModuleWithImports(symbolTable, "main", astModule, availableModules, availableTypes, Set.empty, Set.empty) with
        | { succeeded = false; diagnostics = diagnostics } ->
            Assert.Contains(diagnostics, fun diagnostic -> diagnostic.message.Contains("ambiguous"))
        | _ ->
            Assert.True(false, "Expected import ambiguity diagnostic")

    /// モジュールが存在しない場合、availableTypeFullNames で指定された型 import にフォールバックする。
    [<Fact>]
    let ``Resolve.resolveModule falls back to type import when module does not exist`` () =
        let span = Span.Empty
        let importDecl = Ast.Decl.Import([ "Foo"; "Bar" ], false, span) :> Ast.Decl
        let astModule = Ast.Module([ importDecl ])
        let symbolTable = SymbolTable()

        let availableModules = Set.empty<string>
        let availableTypes = Set.ofList [ "Foo.Bar" ]

        match Resolve.resolveModuleWithImports(symbolTable, "main", astModule, availableModules, availableTypes, Set.empty, Set.empty) with
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
        let importDecl = Ast.Decl.Import([ "Non"; "Existent"; "Type" ], false, span) :> Ast.Decl
        let retType = Ast.TypeExpr.Id("Int", span) :> Ast.TypeExpr
        let fnDecl = Ast.Decl.Fn("main", [], retType, Ast.Expr.Int(0, span) :> Ast.Expr, false, false, span) :> Ast.Decl
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

fn main (): Int
    MyClass.
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

    /// Float（単精度）同士の四則演算がビルトイン演算子解決で成功することを確認する。
    [<Fact>]
    let ``semantic analysis resolves float builtin arithmetic operators`` () =
        let program = """
fn main (): Float
    2.0f * 3.0f + 1.0f
"""
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
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

fn main: ()
    42 Console'WriteLine.
"""
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
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
    let ``semantic analysis auto-injects this for impl instance call`` () =
        let program = """
struct CalculatorWindow
    val value: Int

impl CalculatorWindow
    fn addDigitButton self (digit: Int) (row: Int) (column: Int): Int
        digit

fn main (): Int
    val window = { value = 0 } CalculatorWindow.
    7 0 0 window'addDigitButton.
"""
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
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
    let ``semantic analysis resolves Console WriteLine for impl-method result`` () =
        let program = """
import System'Console

struct Line
    val slope: Double
    val intercept: Double

impl Line
    fn evaluate self (x: Double): Double
        x

fn main (): ()
    val line = { slope = 2.0, intercept = -1.0 } Line.
    5.0 line'evaluate. Console'WriteLine.
"""
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
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
        let fnDecl = Ast.Decl.Fn("main", [], intType, body, false, false, span) :> Ast.Decl
        let astModule = Ast.Module([ fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        let result = Analyze.analyzeModule(symbolTable, subst, "main", astModule)

        Assert.False(result.succeeded)
        Assert.True(result.diagnostics |> List.exists (fun diagnostic -> diagnostic.isError))
        Assert.True(result.value.IsSome, "意味エラー時も IntelliSense 用の部分 HIR を返す必要があります。")

    [<Fact>]
    let ``Bool literal true is analyzed to Hir.Expr.Bool with TypeId.Bool`` () =
        let span = Span.Empty
        let retType = Ast.TypeExpr.Id("Bool", span) :> Ast.TypeExpr
        let body = Ast.Expr.Bool(true, span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, false, false, span) :> Ast.Decl
        let astModule = Ast.Module([fnDecl])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            match hirModule.methods.Head.body with
            | Hir.Expr.Bool (value, _) ->
                Assert.True(value, "expected true")
                Assert.Equal(TypeId.Bool, hirModule.methods.Head.body.typ)
            | other -> Assert.True(false, $"expected Hir.Expr.Bool but got {other}")
        | { diagnostics = diagnostics } ->
            let message =
                diagnostics
                |> List.map (fun err -> err.toDisplayText())
                |> String.concat "; "
            Assert.True(false, $"semantic analysis failed: {message}")

    [<Fact>]
    let ``Bool literal false is analyzed to Hir.Expr.Bool with TypeId.Bool`` () =
        let span = Span.Empty
        let retType = Ast.TypeExpr.Id("Bool", span) :> Ast.TypeExpr
        let body = Ast.Expr.Bool(false, span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], retType, body, false, false, span) :> Ast.Decl
        let astModule = Ast.Module([fnDecl])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            match hirModule.methods.Head.body with
            | Hir.Expr.Bool (value, _) ->
                Assert.False(value, "expected false")
                Assert.Equal(TypeId.Bool, hirModule.methods.Head.body.typ)
            | other -> Assert.True(false, $"expected Hir.Expr.Bool but got {other}")
        | { diagnostics = diagnostics } ->
            let message =
                diagnostics
                |> List.map (fun err -> err.toDisplayText())
                |> String.concat "; "
            Assert.True(false, $"semantic analysis failed: {message}")

    [<Fact>]
    let ``semantic analysis resolves inherited native property read via impl as`` () =
        // System.Exception は非シール・非インターフェイスクラス。Message プロパティを持つ。
        // impl MyError as Exception により MyError 型の変数から .Message を読み取れる必要がある。
        let source = """
import System'Exception
struct MyError
    val code: Int
impl MyError as Exception
    fn new (code: Int): MyError
        { code = code } MyError.
fn getMessage (e: MyError): String
    e'Message
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    let getMessageMethod =
                        hirModule.methods
                        |> List.tryFind (fun methodDef ->
                            match symbolTable.Get(methodDef.sym) with
                            | Some symInfo -> symInfo.name = "getMessage"
                            | None -> false)
                    Assert.True(getMessageMethod.IsSome, "Expected 'getMessage' to be analyzed successfully with native base property read")
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
    let ``semantic analysis resolves inherited native property assignment via impl as`` () =
        // System.Exception.HelpLink は読み書き可能な String プロパティ。
        // impl MyError as Exception により MyError 型の変数への .HelpLink 代入が通る必要がある。
        let source = """
import System'Exception
struct MyError
    val code: Int
impl MyError as Exception
    fn new (code: Int): MyError
        { code = code } MyError.
fn setLink (e: MyError): ()
    e'HelpLink = "https://example.com"
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    let setLinkMethod =
                        hirModule.methods
                        |> List.tryFind (fun methodDef ->
                            match symbolTable.Get(methodDef.sym) with
                            | Some symInfo -> symInfo.name = "setLink"
                            | None -> false)
                    Assert.True(setLinkMethod.IsSome, "Expected 'setLink' to be analyzed successfully with native base property assignment")
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
    let ``semantic analysis resolves base member access inside impl as instance method`` () =
        // Regression: `base'...` must resolve inside impl-as instance methods
        // using current `this` and the declared native base type.
        let source = """
import System'Exception
struct MyError
    val code: Int
impl MyError as Exception
    fn baseText self: String
        base'ToString.
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    // インスタンス impl メソッドは hirType.methods へルーティングされる。
                    let allMethods =
                        hirModule.methods
                        @ (hirModule.types |> List.filter (fun t -> not t.isInterface) |> List.collect (fun t -> t.methods))
                    let baseTextMethod =
                        allMethods
                        |> List.tryFind (fun methodDef ->
                            match symbolTable.Get(methodDef.sym) with
                            | Some symInfo -> symInfo.name = "MyError.baseText"
                            | None -> false)
                    Assert.True(baseTextMethod.IsSome, "Expected 'MyError.baseText' to be analyzed successfully with base member access")
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
    let ``semantic analysis reports error for undefined member on impl as native base type`` () =
        // impl MyError as Exception でも存在しないメンバーへのアクセスはエラーになる必要がある。
        let source = """
import System'Exception
struct MyError
    val code: Int
impl MyError as Exception
    fn new (code: Int): MyError
        { code = code } MyError.
fn bad (e: MyError): String
    e'NonExistentMember
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = false; diagnostics = diagnostics } ->
                    let hasError =
                        diagnostics
                        |> List.exists (fun diagnostic -> diagnostic.isError)
                    Assert.True(hasError, "Expected an error diagnostic for undefined member on impl-as type")
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for undefined member access on impl-as type")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    // ─────────────────────────────────────────────
    // `override` 修飾子のセマンティクステスト
    // ─────────────────────────────────────────────

    [<Fact>]
    let ``semantic analysis accepts override of native virtual method in impl as block`` () =
        // System.Object.ToString は public virtual。MyError.ToString を override できる必要がある。
        let source = """
import System'Exception
struct MyError
    val code: Int
impl MyError as Exception
    fn new (code: Int): MyError
        { code = code } MyError.
    override fn ToString self: String
        "MyError"
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true } -> ()
                | { diagnostics = diagnostics } ->
                    let message =
                        diagnostics
                        |> List.map (fun diagnostic -> diagnostic.toDisplayText())
                        |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed unexpectedly for valid override: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis rejects override of method not present in base class`` () =
        // System.Exception には `NotARealMethod` というメソッドはないので override エラー。
        let source = """
import System'Exception
struct MyError
    val code: Int
impl MyError as Exception
    fn new (code: Int): MyError
        { code = code } MyError.
    override fn NotARealMethod self: Unit
        ()
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = false; diagnostics = diagnostics } ->
                    let combined = diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
                    Assert.Contains("No overridable method 'NotARealMethod'", combined)
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for override of nonexistent method")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis rejects override of non-virtual method in base class`` () =
        // System.Object.GetType は非 virtual（実際は internal virtual に近いが GetMethods のフィルタで除外される）。
        // arity 0 で `GetType` を override しようとすると候補無しエラー。
        let source = """
import System'Exception
struct MyError
    val code: Int
impl MyError as Exception
    fn new (code: Int): MyError
        { code = code } MyError.
    override fn GetType self: String
        "MyError"
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = false; diagnostics = diagnostics } ->
                    let combined = diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
                    Assert.Contains("No overridable method 'GetType'", combined)
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for override of non-virtual method")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis rejects override modifier in plain impl block`` () =
        // `impl A`（as/for なし）の中での override はエラー。
        let source = """
struct Foo
    val x: Int
impl Foo
    override fn bar self: Unit
        ()
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = false; diagnostics = diagnostics } ->
                    let combined = diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
                    Assert.Contains("'override' keyword is only allowed in 'impl ... as ...' blocks", combined)
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for override in plain impl block")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis rejects override modifier in impl for role block`` () =
        // `impl Role for Type` 形式での override はエラー。
        let source = """
import System'IDisposable
struct Box
    val handle: Int
impl IDisposable for Box
    override fn Dispose self: Unit
        ()
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = false; diagnostics = diagnostics } ->
                    let combined = diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
                    Assert.Contains("'override' keyword is only allowed in 'impl ... as ...' blocks", combined)
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for override in 'impl for' block")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis rejects override on static (non-self) method`` () =
        // self を取らない static メソッドへの override はインスタンスメソッド限定の制約に反する。
        let source = """
import System'Exception
struct MyError
    val code: Int
impl MyError as Exception
    override fn ToString (x: Int): String
        "static"
"""

        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = false; diagnostics = diagnostics } ->
                    let combined = diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
                    Assert.Contains("'override' is only allowed on instance methods", combined)
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for override on static method")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    // クロスモジュール `impl X as DotNetBase` テスト共通ヘルパー。
    // ソース文字列を解析してモジュール名と AST のペアを返す。
    let private parseSourceModule (moduleName: string) (source: string) : Result<string * Ast.Module, string> =
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) -> Ok (moduleName, astModule)
            | Failure (reason, _) -> Result.Error (sprintf "Parsing failed in '%s': %s" moduleName reason)
        | Failure (reason, _) -> Result.Error (sprintf "Lexing failed in '%s': %s" moduleName reason)

    // HIR モジュールからモジュールエクスポートマップを構築する（Compile.fs の collectModuleExports に対応する）。
    // `implBase:{TypeName}` エントリを含む、クロスモジュール解析テスト用のヘルパー。
    let private buildModuleExports (symbolTable: SymbolTable) (hirModule: Hir.Module) : Map<string, ModuleExport> =
        let typeExports =
            hirModule.types
            |> List.choose (fun hirType ->
                symbolTable.Get(hirType.sym)
                |> Option.map (fun symInfo ->
                    $"type:{symInfo.name}",
                    { symbolId = hirType.sym; typ = TypeId.Name hirType.sym }))
            |> Map.ofList
        let methodExports =
            hirModule.methods
            |> List.choose (fun methodInfo ->
                symbolTable.Get(methodInfo.sym)
                |> Option.map (fun symInfo ->
                    symInfo.name,
                    { symbolId = methodInfo.sym; typ = symInfo.typ }))
            |> Map.ofList
        let fieldExports =
            hirModule.types
            |> List.collect (fun hirType ->
                hirType.fields
                |> List.choose (fun field ->
                    symbolTable.Get(field.sym)
                    |> Option.map (fun fieldInfo ->
                        $"field:{fieldInfo.name}",
                        { symbolId = field.sym; typ = field.typ })))
            |> Map.ofList
        // impl X as Y で確定した .NET 基底型を "implBase:{TypeName}" キーでエクスポートする。
        let implBaseExports =
            hirModule.types
            |> List.choose (fun hirType ->
                match hirType.baseType with
                | None -> None
                | Some baseType ->
                    symbolTable.Get(hirType.sym)
                    |> Option.map (fun symInfo ->
                        $"implBase:{symInfo.name}",
                        { symbolId = hirType.sym; typ = baseType }))
            |> Map.ofList
        [ typeExports; methodExports; fieldExports; implBaseExports ]
        |> List.fold (fun acc m -> Map.fold (fun a k v -> Map.add k v a) acc m) Map.empty

    /// クロスモジュールの `impl X as DotNetBase` で、import 先からインスタンスプロパティを読み取れることを検証する。
    /// 回帰テスト: import 後に baseType が None になり "does not support member access" が出ていたバグの修正確認。
    [<Fact>]
    let ``cross-module impl as restores base type and resolves native property read`` () =
        // モジュール A: MyError を Exception のサブクラスとして定義する。
        let moduleASource = """
import System'Exception
struct MyError
    val code: Int
impl MyError as Exception
    fn new (code: Int): MyError
        { code = code } MyError.
"""
        // モジュール B: ModuleA から MyError を import して Exception 由来の Message プロパティを読み取る。
        let moduleBSource = """
import ModuleA'MyError
fn getMessage (e: MyError): String
    e'Message
"""
        match parseSourceModule "ModuleA" moduleASource, parseSourceModule "ModuleB" moduleBSource with
        | Ok (nameA, astA), Ok (nameB, astB) ->
            let moduleAsts = Map.ofList [ nameA, astA; nameB, astB ]

            // Compile.fs と同じ方法で利用可能型・impl 宣言マップを構築する。
            let availableModuleNames = moduleAsts |> Map.keys |> Set.ofSeq
            let availableTypeFullNames =
                moduleAsts
                |> Map.toList
                |> List.collect (fun (moduleName, moduleAst) ->
                    moduleAst.decls
                    |> List.choose (fun decl ->
                        match decl with
                        | :? Ast.Decl.Data as dataDecl -> Some $"{moduleName}.{dataDecl.name}"
                        | _ -> None))
                |> Set.ofList
            let availableTypeDecls =
                moduleAsts
                |> Map.toList
                |> List.collect (fun (moduleName, moduleAst) ->
                    moduleAst.decls
                    |> List.choose (fun decl ->
                        match decl with
                        | :? Ast.Decl.Data as dataDecl -> Some ($"{moduleName}.{dataDecl.name}", dataDecl :> Ast.Decl)
                        | _ -> None))
                |> Map.ofList
            let availableDataTypeImplDecls =
                moduleAsts
                |> Map.toList
                |> List.collect (fun (moduleName, moduleAst) ->
                    moduleAst.decls
                    |> List.choose (fun decl ->
                        match decl with
                        | :? Ast.Decl.Impl as implDecl -> Some (moduleName, implDecl)
                        | _ -> None))
                |> List.fold
                    (fun (acc: Map<string, Ast.Decl.Impl list>) (moduleName, implDecl) ->
                        let key = $"{moduleName}.{implDecl.typeName}"
                        match Map.tryFind key acc with
                        | Some impls -> Map.add key (impls @ [ implDecl ]) acc
                        | None -> Map.add key [ implDecl ] acc)
                    Map.empty

            let symbolTable = SymbolTable()
            let typeSubst = TypeSubst()
            let typeMetaFactory = TypeMetaFactory()

            // モジュール A を先に解析し、エクスポートマップを構築する。
            let moduleAResult =
                Analyze.analyzeModuleWithImports(
                    symbolTable, typeSubst, typeMetaFactory, nameA, astA,
                    availableModuleNames, availableTypeFullNames,
                    Set.empty, Set.empty,
                    availableTypeDecls, availableDataTypeImplDecls, Map.empty, Map.empty)

            match moduleAResult with
            | { succeeded = false; diagnostics = diagnostics } ->
                let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                Assert.True(false, $"Module A analysis failed: {message}")
            | { value = Some hirModuleA } ->
                let moduleAExports = buildModuleExports symbolTable hirModuleA

                // モジュール B を解析し、MyError'Message が解決できることを確認する。
                let moduleBResult =
                    Analyze.analyzeModuleWithImports(
                        symbolTable, typeSubst, typeMetaFactory, nameB, astB,
                        availableModuleNames, availableTypeFullNames,
                        Set.empty, Set.empty,
                        availableTypeDecls, availableDataTypeImplDecls,
                        Map.ofList [ nameA, moduleAExports ], Map.empty)

                match moduleBResult with
                | { succeeded = true } -> ()
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Module B analysis failed (cross-module impl-as property read): {message}")
            | { value = None } ->
                Assert.True(false, "Module A analysis returned no HIR module")
        | Result.Error message, _ | _, Result.Error message ->
            Assert.True(false, $"Parse error: {message}")

    /// クロスモジュールの `impl X as DotNetBase` で、import 先からプロパティへ代入できることを検証する。
    [<Fact>]
    let ``cross-module impl as restores base type and resolves native property assignment`` () =
        // モジュール A: MyError を Exception のサブクラスとして定義する。
        let moduleASource = """
import System'Exception
struct MyError
    val code: Int
impl MyError as Exception
    fn new (code: Int): MyError
        { code = code } MyError.
"""
        // モジュール B: ModuleA から MyError を import して HelpLink（読み書き可能）へ代入する。
        let moduleBSource = """
import ModuleA'MyError
fn setLink (e: MyError): ()
    e'HelpLink = "https://example.com"
"""
        match parseSourceModule "ModuleA" moduleASource, parseSourceModule "ModuleB" moduleBSource with
        | Ok (nameA, astA), Ok (nameB, astB) ->
            let moduleAsts = Map.ofList [ nameA, astA; nameB, astB ]
            let availableModuleNames = moduleAsts |> Map.keys |> Set.ofSeq
            let availableTypeFullNames =
                moduleAsts
                |> Map.toList
                |> List.collect (fun (moduleName, moduleAst) ->
                    moduleAst.decls
                    |> List.choose (fun decl ->
                        match decl with
                        | :? Ast.Decl.Data as dataDecl -> Some $"{moduleName}.{dataDecl.name}"
                        | _ -> None))
                |> Set.ofList
            let availableTypeDecls =
                moduleAsts
                |> Map.toList
                |> List.collect (fun (moduleName, moduleAst) ->
                    moduleAst.decls
                    |> List.choose (fun decl ->
                        match decl with
                        | :? Ast.Decl.Data as dataDecl -> Some ($"{moduleName}.{dataDecl.name}", dataDecl :> Ast.Decl)
                        | _ -> None))
                |> Map.ofList
            let availableDataTypeImplDecls =
                moduleAsts
                |> Map.toList
                |> List.collect (fun (moduleName, moduleAst) ->
                    moduleAst.decls
                    |> List.choose (fun decl ->
                        match decl with
                        | :? Ast.Decl.Impl as implDecl -> Some (moduleName, implDecl)
                        | _ -> None))
                |> List.fold
                    (fun (acc: Map<string, Ast.Decl.Impl list>) (moduleName, implDecl) ->
                        let key = $"{moduleName}.{implDecl.typeName}"
                        match Map.tryFind key acc with
                        | Some impls -> Map.add key (impls @ [ implDecl ]) acc
                        | None -> Map.add key [ implDecl ] acc)
                    Map.empty

            let symbolTable = SymbolTable()
            let typeSubst = TypeSubst()
            let typeMetaFactory = TypeMetaFactory()

            let moduleAResult =
                Analyze.analyzeModuleWithImports(
                    symbolTable, typeSubst, typeMetaFactory, nameA, astA,
                    availableModuleNames, availableTypeFullNames,
                    Set.empty, Set.empty,
                    availableTypeDecls, availableDataTypeImplDecls, Map.empty, Map.empty)

            match moduleAResult with
            | { succeeded = false; diagnostics = diagnostics } ->
                let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                Assert.True(false, $"Module A analysis failed: {message}")
            | { value = Some hirModuleA } ->
                let moduleAExports = buildModuleExports symbolTable hirModuleA

                let moduleBResult =
                    Analyze.analyzeModuleWithImports(
                        symbolTable, typeSubst, typeMetaFactory, nameB, astB,
                        availableModuleNames, availableTypeFullNames,
                        Set.empty, Set.empty,
                        availableTypeDecls, availableDataTypeImplDecls,
                        Map.ofList [ nameA, moduleAExports ], Map.empty)

                match moduleBResult with
                | { succeeded = true } -> ()
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Module B analysis failed (cross-module impl-as property assignment): {message}")
            | { value = None } ->
                Assert.True(false, "Module A analysis returned no HIR module")
        | Result.Error message, _ | _, Result.Error message ->
            Assert.True(false, $"Parse error: {message}")

    /// 回帰テスト: `impl X as DotNetBase` 型を引数に取る関数へ X 型の値を渡せる。
    /// unifyOrError 内の isSubtypeOfSystemType が TypeId.Native ベース型を辿れることを検証する (Fix 1)。
    [<Fact>]
    let ``semantic analysis accepts impl-as data type passed as native base type function argument`` () =
        // getMsg は Exception 型の引数を取る Atla 関数。
        // MyError は impl MyError as Exception を宣言しているため
        // MyError の値を getMsg に渡せなければならない。
        let source = """
import System'Exception
struct MyError
    val code: Int
impl MyError as Exception
    fn new (code: Int): MyError
        { code = code } MyError.
fn getMsg (e: Exception): String
    e'Message
fn test (): String
    val err = { code = 42 } MyError.
    err getMsg.
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    let testMethod =
                        hirModule.methods
                        |> List.tryFind (fun methodDef ->
                            match symbolTable.Get(methodDef.sym) with
                            | Some symInfo -> symInfo.name = "test"
                            | None -> false)
                    Assert.True(testMethod.IsSome, "Expected 'test' to succeed: impl-as type passed as native base type argument")
                | { diagnostics = diagnostics } ->
                    let message =
                        diagnostics
                        |> List.map (fun d -> d.toDisplayText())
                        |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    /// 回帰テスト: `impl X as DotNetBase` 型を .NET 静的メソッドのオーバーロード解決で正しく選択する。
    /// isSubtypeCompatible 内の isNameSubtypeOfNative が TypeId.Native ベース型を辿れることを検証する (Fix 2)。
    [<Fact>]
    let ``semantic analysis resolves native static method overload for impl-as data type argument`` () =
        // ExceptionDispatchInfo.Capture(Exception) は Exception 引数を取る静的メソッド。
        // impl MyError as Exception により MyError :< Exception が成立するため Capture が選択される。
        let source = """
import System'Exception
import System'Runtime'ExceptionServices'ExceptionDispatchInfo
struct MyError
    val code: Int
impl MyError as Exception
    fn new (code: Int): MyError
        { code = code } MyError.
fn test (): ExceptionDispatchInfo
    val err = { code = 1 } MyError.
    err ExceptionDispatchInfo'Capture.
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    let testMethod =
                        hirModule.methods
                        |> List.tryFind (fun methodDef ->
                            match symbolTable.Get(methodDef.sym) with
                            | Some symInfo -> symInfo.name = "test"
                            | None -> false)
                    Assert.True(testMethod.IsSome, "Expected 'test' to succeed: impl-as type passed to native static method via overload resolution")
                | { diagnostics = diagnostics } ->
                    let message =
                        diagnostics
                        |> List.map (fun d -> d.toDisplayText())
                        |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``lambda analyzed as specific delegate type preserves that delegate type in HIR`` () =
        // 回帰テスト: EventHandler<T> などの具体的なデリゲート型を期待する位置にラムダを渡した場合、
        // Lambda の HIR 型は TypeId.Fn(...) ではなく TypeId.Native(EventHandler<T>) になることを検証する。
        // これにより Layout フェーズが正確なデリゲート型で FnDelegate を生成し、
        // InvalidCastException（Action<obj,T> vs EventHandler<T>）を防ぐ。
        let input: Input<SourceChar> = StringInput """
import System'AppDomain

fn test (domain: AppDomain): ()
    domain'ProcessExit += fn _ __ -> ()
"""
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (moduleAst, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "test", moduleAst) with
                | { succeeded = true; value = Some hirModule } ->
                    // test メソッドの本体を取得する。
                    let testMethod =
                        hirModule.methods
                        |> List.tryFind (fun m ->
                            match symbolTable.Get(m.sym) with
                            | Some info -> info.name = "test"
                            | None -> false)
                    match testMethod with
                    | None -> Assert.True(false, "method 'test' not found in HIR module")
                    | Some methodInfo ->
                        // do ブロック内の ExprStmt (add_ProcessExit 呼び出し) を探す。
                        let findLambdaArgType (stmts: Hir.Stmt list) =
                            stmts
                            |> List.tryPick (fun stmt ->
                                match stmt with
                                | Hir.Stmt.ExprStmt (Hir.Expr.Call (_, _, args, _, _), _) ->
                                    // 引数の中から Lambda を探す。
                                    args |> List.tryPick (fun arg ->
                                        match arg with
                                        | Hir.Expr.Lambda (_, _, _, lambdaTid, _) -> Some lambdaTid
                                        | _ -> None)
                                | _ -> None)
                        let lambdaTypeOpt =
                            match methodInfo.body with
                            | Hir.Expr.Block (stmts, _, _, _) -> findLambdaArgType stmts
                            | _ -> None
                        match lambdaTypeOpt with
                        | None -> Assert.True(false, "Lambda not found in HIR method body")
                        | Some lambdaTid ->
                            // Lambda の型が TypeId.Native(EventHandler) であることを確認する。
                            // TypeId.Fn(...) のままだと Layout フェーズで Action<,> が使われ InvalidCastException になる。
                            let expectedDelegateType = typeof<System.EventHandler>
                            Assert.Equal(TypeId.Native expectedDelegateType, lambdaTid)
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``compound add assignment lowers to builtin operator and assign`` () =
        let span = Span.Empty
        let body =
            Ast.Expr.Block([
                Ast.Stmt.Var("x", None, Ast.Expr.Int(1, span) :> Ast.Expr, span) :> Ast.Stmt
                Ast.Stmt.CompoundAssign(Ast.Stmt.CompoundAssignOp.Add, Ast.Expr.Id("x", span) :> Ast.Expr, Ast.Expr.Int(2, span) :> Ast.Expr, span) :> Ast.Stmt
            ], span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], Ast.TypeExpr.Unit(span) :> Ast.TypeExpr, body, false, false, span) :> Ast.Decl
        let astModule = Ast.Module([ fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            match hirModule.methods.Head.body with
            | Hir.Expr.Block(stmts, _, _, _) ->
                match stmts |> List.tryLast with
                | Some (Hir.Stmt.Assign(_, Hir.Expr.Call(Hir.Callable.BuiltinOperator _, _, _, _, _), _)) -> Assert.True(true)
                | _ -> Assert.True(false, "expected compound assignment to become builtin operator assign")
            | _ -> Assert.True(false, "expected block body")
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun err -> err.toDisplayText()) |> String.concat "; "
            Assert.True(false, $"semantic analysis failed: {message}")

    [<Fact>]
    let ``compound mul assignment lowers to builtin operator and assign`` () =
        let span = Span.Empty
        let body =
            Ast.Expr.Block([
                Ast.Stmt.Var("x", None, Ast.Expr.Int(3, span) :> Ast.Expr, span) :> Ast.Stmt
                Ast.Stmt.CompoundAssign(Ast.Stmt.CompoundAssignOp.Mul, Ast.Expr.Id("x", span) :> Ast.Expr, Ast.Expr.Int(4, span) :> Ast.Expr, span) :> Ast.Stmt
            ], span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], Ast.TypeExpr.Unit(span) :> Ast.TypeExpr, body, false, false, span) :> Ast.Decl
        let astModule = Ast.Module([ fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            match hirModule.methods.Head.body with
            | Hir.Expr.Block(stmts, _, _, _) ->
                match stmts |> List.tryLast with
                | Some (Hir.Stmt.Assign(_, Hir.Expr.Call(Hir.Callable.BuiltinOperator Builtins.Operators.OpMul, _, _, _, _), _)) -> Assert.True(true)
                | _ -> Assert.True(false, "expected compound *= to become OpMul builtin operator assign")
            | _ -> Assert.True(false, "expected block body")
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun err -> err.toDisplayText()) |> String.concat "; "
            Assert.True(false, $"semantic analysis failed: {message}")

    [<Fact>]
    let ``compound div assignment lowers to builtin operator and assign`` () =
        let span = Span.Empty
        let body =
            Ast.Expr.Block([
                Ast.Stmt.Var("x", None, Ast.Expr.Int(10, span) :> Ast.Expr, span) :> Ast.Stmt
                Ast.Stmt.CompoundAssign(Ast.Stmt.CompoundAssignOp.Div, Ast.Expr.Id("x", span) :> Ast.Expr, Ast.Expr.Int(2, span) :> Ast.Expr, span) :> Ast.Stmt
            ], span) :> Ast.Expr
        let fnDecl = Ast.Decl.Fn("main", [], Ast.TypeExpr.Unit(span) :> Ast.TypeExpr, body, false, false, span) :> Ast.Decl
        let astModule = Ast.Module([ fnDecl ])

        let symbolTable = SymbolTable()
        let subst = TypeSubst()
        match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
        | { succeeded = true; value = Some hirModule } ->
            match hirModule.methods.Head.body with
            | Hir.Expr.Block(stmts, _, _, _) ->
                match stmts |> List.tryLast with
                | Some (Hir.Stmt.Assign(_, Hir.Expr.Call(Hir.Callable.BuiltinOperator Builtins.Operators.OpDiv, _, _, _, _), _)) -> Assert.True(true)
                | _ -> Assert.True(false, "expected compound /= to become OpDiv builtin operator assign")
            | _ -> Assert.True(false, "expected block body")
        | { diagnostics = diagnostics } ->
            let message = diagnostics |> List.map (fun err -> err.toDisplayText()) |> String.concat "; "
            Assert.True(false, $"semantic analysis failed: {message}")

    /// 回帰テスト: `Float'Parse` — ビルトイン型を静的メンバーアクセスのレシーバとして使う場合に
    /// "Undefined variable 'Float'" が誤報告されなかったことを検証する。
    [<Fact>]
    let ``Float'Parse resolves to System.Single.Parse without error`` () =
        let program = """
fn parse (s: String): Float
    s Float'Parse.
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "HIR に ExprError/ErrorStmt が残っています。")
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    /// 回帰テスト: `Int'Parse` — ビルトイン型 Int に対しても同様に静的メンバーアクセスが解決できることを検証する。
    [<Fact>]
    let ``Int'Parse resolves to System.Int32.Parse without error`` () =
        let program = """
fn parse (s: String): Int
    s Int'Parse.
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
                    let hasError = hirModule.methods |> List.exists (fun m -> m.hasError)
                    Assert.False(hasError, "HIR に ExprError/ErrorStmt が残っています。")
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``role declaration produces isInterface HIR type`` () =
        // role 宣言が isInterface=true の Hir.Type として解析されることを検証する。
        let source = """
role Geometry
    fn area self: Float
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    let geometryType =
                        hirModule.types
                        |> List.tryFind (fun t ->
                            match symbolTable.Get(t.sym) with
                            | Some symInfo -> symInfo.name = "Geometry"
                            | None -> false)
                    Assert.True(geometryType.IsSome, "Expected 'Geometry' HIR type to exist")
                    Assert.True(geometryType.Value.isInterface, "Expected 'Geometry' HIR type to be an interface")
                    Assert.Equal(1, geometryType.Value.methods.Length)
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``role and impl for analyzed without errors`` () =
        // role 宣言と impl ... for ... の組み合わせが解析エラーなく通ることを検証する。
        let source = """
role Geometry
    fn area self: Float

struct Rectangle
    val width: Float
    val height: Float

impl Geometry for Rectangle
    fn area self: Float
        self'width * self'height
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    Assert.False(hirModule.hasError, "HIR に ExprError/ErrorStmt が残っています。")
                    let geometryType =
                        hirModule.types
                        |> List.tryFind (fun t ->
                            match symbolTable.Get(t.sym) with
                            | Some symInfo -> symInfo.name = "Geometry"
                            | None -> false)
                    Assert.True(geometryType.IsSome, "Expected 'Geometry' HIR type to exist")
                    Assert.True(geometryType.Value.isInterface, "Expected 'Geometry' HIR type to be an interface")
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``generic enum declaration produces HIR type with typeParams`` () =
        // `enum Opt T` が typeParams=["T"] を持つ Hir.Type として解析されることを検証する。
        let source = """
enum Opt T
    | None
    | Some { value: T }
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    let optType =
                        hirModule.types
                        |> List.tryFind (fun t ->
                            match symbolTable.Get(t.sym) with
                            | Some symInfo -> symInfo.name = "Opt"
                            | None -> false)
                    Assert.True(optType.IsSome, "Expected 'Opt' HIR type to exist")
                    Assert.Equal<string list>(["T"], optType.Value.typeParams)
                    let payloadType =
                        hirModule.types
                        |> List.tryFind (fun t ->
                            match symbolTable.Get(t.sym) with
                            // ペイロード型名は "Opt.__enum_payload_Some_type" の形式。
                            | Some symInfo -> symInfo.name.Contains("__enum_payload_Some")
                            | None -> false)
                    match payloadType with
                    | Some p ->
                        Assert.Equal<string list>(["T"], p.typeParams)
                        // フィールド value の型が TypeId.TypeVar "T" であることを確認する。
                        match p.fields |> List.tryFind (fun f -> symbolTable.Get(f.sym) |> Option.map (fun s -> s.name.EndsWith(".value")) |> Option.defaultValue false) with
                        | Some field -> Assert.Equal(TypeId.TypeVar "T", field.typ)
                        | None -> Assert.True(false, "Expected 'value' field in payload type")
                    | None -> Assert.True(false, "Expected payload type for Some case")
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``generic enum with impl analyzes without errors`` () =
        // ジェネリック enum + impl の組み合わせが解析エラーなく通ることを検証する。
        let source = """
enum Opt T
    | None
    | Some { value: T }

impl Opt T
    fn isSome self: Bool
        match self
        | Opt'None -> False
        | Opt'Some { value } -> True

    fn isNone self: Bool
        match self
        | Opt'None -> True
        | Opt'Some { value } -> False
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    Assert.False(hirModule.hasError, "HIR に ExprError/ErrorStmt が残っています。")
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``async fn returning Unit wraps to Task`` () =
        let source = """
async fn run (): ()
    ()
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    let runMethod = hirModule.methods |> List.find (fun m -> m.isAsync)
                    Assert.True(runMethod.isAsync, "method should be flagged isAsync")
                    Assert.False(runMethod.hasError, "async fn returning Unit should have no diagnostics")
                    match Type.resolve subst runMethod.typ with
                    | TypeId.Fn(_, ret) ->
                        Assert.Equal(TypeId.Native typeof<System.Threading.Tasks.Task>, ret)
                    | other -> Assert.True(false, $"expected Fn type, got {other}")
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``async fn returning Int wraps to Task of Int`` () =
        let source = """
async fn good (): Int
    1
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    let runMethod = hirModule.methods |> List.find (fun m -> m.isAsync)
                    Assert.False(runMethod.hasError, "async fn returning Int should have no diagnostics")
                    match Type.resolve subst runMethod.typ with
                    | TypeId.Fn(_, ret) ->
                        Assert.Equal(
                            TypeId.App(TypeId.Native typeof<System.Threading.Tasks.Task>, [ TypeId.Int ]),
                            ret)
                    | other -> Assert.True(false, $"expected Fn type, got {other}")
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``await outside async fn produces diagnostic`` () =
        let source = """
import System'Threading'Tasks'Task

fn sync (t: Task): ()
    await t
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                let result = Analyze.analyzeModule(symbolTable, subst, "main", astModule)
                let hasError =
                    match result.value with
                    | Some hirModule -> hirModule.hasError
                    | None -> not (List.isEmpty result.diagnostics)
                Assert.True(hasError, "non-async fn using await should produce a diagnostic")
                let combinedText =
                    let modDiagText =
                        result.value
                        |> Option.map (fun m -> m.getDiagnostics |> List.map (fun d -> d.message) |> String.concat "; ")
                        |> Option.defaultValue ""
                    let topDiagText = result.diagnostics |> List.map (fun d -> d.message) |> String.concat "; "
                    modDiagText + ";" + topDiagText
                Assert.Contains("await", combinedText)
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``await on Task produces Unit result type`` () =
        let source = """
import System'Threading'Tasks'Task

async fn run (t: Task): ()
    await t
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    let runMethod = hirModule.methods |> List.find (fun m -> m.isAsync)
                    Assert.False(runMethod.hasError, "method should have no diagnostics")
                    let rec findAwait (e: Hir.Expr) : Hir.Expr option =
                        match e with
                        | Hir.Expr.Await _ -> Some e
                        | Hir.Expr.Block (stmts, body, _, _) ->
                            let fromStmts =
                                stmts
                                |> List.tryPick (fun s ->
                                    match s with
                                    | Hir.Stmt.ExprStmt (ee, _) -> findAwait ee
                                    | Hir.Stmt.Let (_, _, ee, _) -> findAwait ee
                                    | _ -> None)
                            match fromStmts with
                            | Some _ -> fromStmts
                            | None -> findAwait body
                        | _ -> None
                    match findAwait runMethod.body with
                    | Some (Hir.Expr.Await (_, resultTid, _)) ->
                        Assert.Equal(TypeId.Unit, Type.resolve subst resultTid)
                    | _ -> Assert.True(false, "expected Hir.Expr.Await in body")
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``await on non-Task operand produces diagnostic`` () =
        let source = """
async fn bad (x: Int): Int
    await x
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                let result = Analyze.analyzeModule(symbolTable, subst, "main", astModule)
                let hasError =
                    match result.value with
                    | Some hirModule -> hirModule.hasError
                    | None -> not (List.isEmpty result.diagnostics)
                Assert.True(hasError, "await on a non-Task operand should produce a diagnostic")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    /// メソッド本体ブロックの先頭の let 文の束縛シンボル型を解決して返す。
    let private firstLetBindingType (symbolTable: SymbolTable) (subst: TypeSubst) (hirModule: Hir.Module) : TypeId option =
        match hirModule.methods.Head.body with
        | Hir.Expr.Block(stmts, _, _, _) ->
            stmts
            |> List.tryPick (fun s -> match s with | Hir.Stmt.Let(sid, _, _, _) -> Some sid | _ -> None)
            |> Option.bind symbolTable.Get
            |> Option.map (fun info -> Type.resolve subst info.typ)
        | _ -> None

    let private assertListInt (typOpt: TypeId option) =
        match typOpt with
        | Some (TypeId.App(TypeId.Native t, [ TypeId.Int ])) when t = typedefof<System.Collections.Generic.List<_>> ->
            Assert.True(true)
        | other -> Assert.True(false, $"expected val binding type List<Int>, got {other}")

    [<Fact>]
    let ``let binding type annotation makes inferred type concrete`` () =
        let source = """
fn main: List Int
    val x: List Int = List.
    x
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    assertListInt (firstLetBindingType symbolTable subst hirModule)
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``expression type ascription makes inferred type concrete`` () =
        let source = """
fn main: List Int
    val y = List. : List Int
    y
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                match Analyze.analyzeModule(symbolTable, subst, "main", astModule) with
                | { succeeded = true; value = Some hirModule } ->
                    assertListInt (firstLetBindingType symbolTable subst hirModule)
                | { diagnostics = diagnostics } ->
                    let message = diagnostics |> List.map (fun d -> d.toDisplayText()) |> String.concat "; "
                    Assert.True(false, $"Semantic analysis failed: {message}")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``type annotation mismatch produces diagnostic`` () =
        let source = """
fn main: Int
    val x: Int = List.
    x
"""
        let input: Input<SourceChar> = StringInput source
        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            let start = if List.isEmpty tokens then Position.Zero else tokens.Head.span.left
            match Parser.fileModule tokenInput start with
            | Success (astModule, _) ->
                let symbolTable = SymbolTable()
                let subst = TypeSubst()
                let result = Analyze.analyzeModule(symbolTable, subst, "main", astModule)
                let hasError =
                    match result.value with
                    | Some hirModule -> hirModule.hasError
                    | None -> not (List.isEmpty result.diagnostics)
                Assert.True(hasError, "annotation/expression type mismatch should produce a diagnostic")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``semantic analysis reports non exhaustive match for nested union leaf`` () =
        let source = """
union Color
    union HueColor: Color
        struct Hsv: HueColor
            val v: Int
        struct Hsl: HueColor
            val l: Int

fn pick (c: Color): Int
    match c
    | Color'HueColor'Hsv { v, .. } -> v
"""
        let input: Input<SourceChar> = StringInput source
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
                    Assert.Contains(diagnostics, fun diagnostic -> diagnostic.message.Contains("Non-exhaustive match") && diagnostic.message.Contains("Hsl"))
                | _ ->
                    Assert.True(false, "Semantic analysis unexpectedly succeeded for non-exhaustive nested union match")
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
