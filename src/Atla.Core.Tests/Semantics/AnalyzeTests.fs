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

    [<Fact>]
    let ``calling non-callable identifier should include type and name in diagnostic`` () =
        // 引数 `x` は Int 型と明示されており、呼び出し不可能。
        // エラーメッセージに引数名と実際の型（Int）が含まれることを確認する。
        let program = """
fn bad (x: Int): Int = x ()
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
                    // HIR 診断を収集して検証する
                    let diagnostics = hirModule.getDiagnostics
                    let notCallableMsg =
                        diagnostics
                        |> List.tryFind (fun d ->
                            let text = d.toDisplayText()
                            text.Contains("not callable") && text.Contains("'x'") && text.Contains("Int"))
                    Assert.True(notCallableMsg.IsSome, sprintf "Expected 'not callable' diagnostic containing name and type, but got: %A" (diagnostics |> List.map (fun d -> d.toDisplayText())))
                | { diagnostics = diagnostics } ->
                    // フェーズレベル診断にある場合はそちらを検証する
                    let notCallableMsg =
                        diagnostics
                        |> List.tryFind (fun d ->
                            let text = d.toDisplayText()
                            text.Contains("not callable") && text.Contains("'x'") && text.Contains("Int"))
                    Assert.True(notCallableMsg.IsSome, sprintf "Expected 'not callable' diagnostic containing name and type, but got: %A" (diagnostics |> List.map (fun d -> d.toDisplayText())))
            | Failure (reason, span) ->
                Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
        | Failure (reason, span) ->
            Assert.True(false, $"Lexing failed: {reason} at {span.left.Line}:{span.left.Column}")
