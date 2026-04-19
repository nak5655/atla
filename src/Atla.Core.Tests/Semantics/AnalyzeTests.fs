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
                    match Layout.layoutAssembly("TestAsm", Hir.Assembly("test", [ hirModule ])) with
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
    let ``nullary function call with unit argument syntax should be allowed`` () =
        let program = """
import System.Console

fn greet (): () = Console.WriteLine "hello!"

fn main: () = greet ()
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
                    Assert.False(hasError, "`greet ()` の0引数呼び出し解析に失敗しています。")
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
    let ``for statement accepts Enumerable.Range enumerable`` () =
        let program = """
import System.Console

import System.Linq.Enumerable

fn main: () = do
    for i in Enumerable.Range 1 20
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
import System.Int32
import System.Console

fn main: () = do
    var n_x = Console.ReadLine ()
    n_x = n_x.Split " "
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
import System.Text.StringBuilder

fn appendExclamation (sb: StringBuilder): () = do
    let ignored = sb.Append "!"
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
import System.Console

fn main: () = do
    let a = (Console.ReadLine ()).Split " "
    Console.WriteLine a !! 0
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
import System.Int32
import System.Console
import System.Linq.Enumerable

fn main: () = do
    let a = (Console.ReadLine ()).Split " "
    for i in Enumerable.Range 0 a.Length
        Console.WriteLine a !! i
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
import System.Activator

fn main (): Int = Activator.CreateInstance[Int] ()
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
import System.Text.StringBuilder

fn createBuilder (): StringBuilder = StringBuilder ()
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
    let ``extension method call should be analyzed`` () =
        let program = """
import Atla.Core.Tests.Semantics.TestExtensions

fn addTen (): () = do
    let x = 1
    x.PlusTen ()
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
fn apply (f: Int -> Int) (x: Int): Int = f x
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
fn apply (f: Int -> Int) (x: Int): Int = f x
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
                    match Layout.layoutAssembly("TestAsm", hirAssembly) with
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
fn apply (f: Int -> Int) (x: Int): Int = f x
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
