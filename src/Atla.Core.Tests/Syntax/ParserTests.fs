namespace Atla.Core.Tests.Syntax

open Xunit
open Atla.Core.Data
open Atla.Core.Syntax
open Atla.Core.Syntax.Data

module ParserTests =
    let private parseModule (program: string) =
        let input: Input<SourceChar> = StringInput program

        match Lexer.tokenize input Position.Zero with
        | Success (tokens, _) ->
            let tokenInput = TokenInput(tokens)
            Parser.fileModule() tokenInput tokens.Head.span.left
        | Failure (reason, span) ->
            Failure ($"Lexing failed: {reason}", span)

    [<Fact>]
    let ``fileModule parses function declaration`` () =
        let program = "fn main (): Int = 1"

        match parseModule program with
        | Success (astModule, _) ->
            Assert.Single(astModule.decls) |> ignore
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses if expression`` () =
        let program = "fn main (): Int = if | 1 == 1 => 1 | else => 0"

        match parseModule program with
        | Success (astModule, _) ->
            Assert.Single(astModule.decls) |> ignore
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses for statement in do block`` () =
        let program = """
fn main: () = do
    for i in values
        i
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn -> Some fn
                    | _ -> None
                )

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    let forStmt =
                        blockExpr.stmts
                        |> List.tryPick (fun stmt ->
                            match stmt with
                            | :? Ast.Stmt.For as forStmt -> Some forStmt
                            | _ -> None
                        )

                    match forStmt with
                    | Some stmt ->
                        Assert.Equal("i", stmt.varName)
                        Assert.Single(stmt.body) |> ignore
                    | None ->
                        Assert.True(false, "for statement was not parsed into Ast.Stmt.For")
                | _ ->
                    Assert.True(false, "function body was not parsed into a block expression")
            | None ->
                Assert.True(false, "function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses fizzbuzz for statement`` () =
        let program = """
fn fizzbuzz (n: Int): () =
    for i in n
        if
            | i % 15 == 0 => "FizzBuzz"
            | i % 5 == 0 => "Buzz"
            | i % 3 == 0 => "Fizz"
            | else => i

fn main: () = do
    let n = 10
    n fizzbuzz.
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn -> Some fn
                    | _ -> None
                )

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    let forStmt =
                        blockExpr.stmts
                        |> List.tryPick (fun stmt ->
                            match stmt with
                            | :? Ast.Stmt.For as forStmt -> Some forStmt
                            | _ -> None
                        )

                    match forStmt with
                    | Some stmt ->
                        Assert.Equal("i", stmt.varName)
                        Assert.Single(stmt.body) |> ignore
                    | None ->
                        Assert.True(false, "for statement was not parsed into Ast.Stmt.For")
                | _ ->
                    Assert.True(false, "function body was not parsed into a block expression")
            | None ->
                Assert.True(false, "function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses index access expression with bang-bang operator`` () =
        let program = """
import System'Console

fn main: () = do
    let line = Console'ReadLine.
    let a = " " line'Split.
    a !! 0 Console'WriteLine.
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    match List.tryLast blockExpr.stmts with
                    | Some (:? Ast.Stmt.ExprStmt as exprStmt) ->
                        let rec containsIndexAccess (expr: Ast.Expr) : bool =
                            match expr with
                            | :? Ast.Expr.IndexAccess -> true
                            | :? Ast.Expr.Apply as applyExpr ->
                                containsIndexAccess applyExpr.func || (applyExpr.args |> List.exists containsIndexAccess)
                            | :? Ast.Expr.MemberAccess as memberExpr ->
                                containsIndexAccess memberExpr.receiver
                            | :? Ast.Expr.GenericApply as genericExpr ->
                                containsIndexAccess genericExpr.func
                            | :? Ast.Expr.Block as blockExpr ->
                                blockExpr.stmts
                                |> List.exists (fun stmt ->
                                    match stmt with
                                    | :? Ast.Stmt.ExprStmt as nestedExprStmt -> containsIndexAccess nestedExprStmt.expr
                                    | :? Ast.Stmt.Let as letStmt -> containsIndexAccess letStmt.value
                                    | :? Ast.Stmt.Var as varStmt -> containsIndexAccess varStmt.value
                                    | :? Ast.Stmt.Assign as assignStmt -> containsIndexAccess assignStmt.value
                                    | :? Ast.Stmt.Return as returnStmt -> containsIndexAccess returnStmt.expr
                                    | _ -> false)
                            | _ -> false

                        Assert.True(containsIndexAccess exprStmt.expr, "index access was not found in parsed expression tree")
                    | _ -> Assert.True(false, "block does not end with expression statement")
                | _ -> Assert.True(false, "main body was not parsed into block expression")
            | None -> Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses generic apply postfix`` () =
        let program = """
import Avalonia'Controls'AppBuilder
import Avalonia'Application

fn main: () = do
    let config = AppBuilder'Configure[Application].
    config
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    let letStmt =
                        blockExpr.stmts
                        |> List.tryPick (fun stmt ->
                            match stmt with
                            | :? Ast.Stmt.Let as stmt when stmt.name = "config" -> Some stmt
                            | _ -> None)

                    match letStmt with
                    | Some stmt ->
                        match stmt.value with
                        | :? Ast.Expr.Apply as applyExpr ->
                            match applyExpr.func with
                            | :? Ast.Expr.GenericApply as genericApply ->
                                match genericApply.typeArgs with
                                | [ (:? Ast.TypeExpr.Id as typeArg) ] ->
                                    Assert.Equal("Application", typeArg.name)
                                | _ ->
                                    Assert.True(false, "generic apply type arguments were not parsed as expected")
                            | _ ->
                                Assert.True(false, "generic apply function was not parsed as Ast.Expr.GenericApply")
                        | _ ->
                            Assert.True(false, "let statement value was not parsed as apply expression")
                    | None ->
                        Assert.True(false, "config let statement was not found")
                | _ ->
                    Assert.True(false, "main body was not parsed into a block expression")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses record-style data declaration with comma-separated fields`` () =
        let program = "data Person = { name: String, age: Int }"

        match parseModule program with
        | Success (astModule, _) ->
            match astModule.decls with
            | [ (:? Ast.Decl.Data as dataDecl) ] ->
                Assert.Equal("Person", dataDecl.name)
                Assert.Equal(2, dataDecl.items.Length)
            | _ ->
                Assert.True(false, "expected a single Ast.Decl.Data declaration")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses named data initialization expression`` () =
        let program = """
data Person = { name: String, age: Int }
fn main (): Person = Person { name = "Alice", age = 20 }
"""

        match parseModule program with
        | Success (astModule, _) ->
            let mainDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match mainDecl with
            | Some fnDecl ->
                match fnDecl.body with
                | :? Ast.Expr.DataInit as initExpr ->
                    Assert.Equal("Person", initExpr.typeName)
                    Assert.Equal(2, initExpr.fields.Length)
                | _ ->
                    Assert.True(false, "expected Ast.Expr.DataInit in main body")
            | None ->
                Assert.True(false, "main declaration not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses apostrophe import syntax`` () =
        let program = """
import System'Console

fn main: () = ()
"""

        match parseModule program with
        | Success _ -> Assert.True(true)
        | Failure (reason, span) ->
            Assert.True(false, $"apostrophe import syntax should be accepted: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses unary minus float literal in data initialization`` () =
        let program = """
data Line = { slope: Float, intercept: Float }
fn main (): Line = Line { slope = 2.0, intercept = -1.0 }
"""

        match parseModule program with
        | Success (astModule, _) ->
            let mainDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match mainDecl with
            | Some fnDecl ->
                match fnDecl.body with
                | :? Ast.Expr.DataInit as initExpr ->
                    match initExpr.fields |> List.tryFind (fun field -> match field with | :? Ast.DataInitField.Field as f -> f.name = "intercept" | _ -> false) with
                    | Some (:? Ast.DataInitField.Field as interceptField) ->
                        match interceptField.value with
                        | :? Ast.Expr.Float as floatExpr ->
                            Assert.Equal(-1.0, floatExpr.value)
                        | _ ->
                            Assert.True(false, "intercept field was not parsed as Ast.Expr.Float")
                    | _ ->
                        Assert.True(false, "intercept field initializer was not found")
                | _ ->
                    Assert.True(false, "main body was not parsed as Ast.Expr.DataInit")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses consecutive exprStmts in do block as separate statements`` () =
        // 回帰テスト: 同じインデントレベルの連続する exprStmt が 1 つの Apply 式として誤解析されないことを確認する。
        let program = """
import System'Console

fn main (): () = do
    "hello" Console'WriteLine.
    "world" Console'WriteLine.
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    // 2 つの文がそれぞれ別の exprStmt として解析されていることを確認する。
                    Assert.Equal(2, blockExpr.stmts.Length)

                    let rec containsExpectedStringArg (expr: Ast.Expr) (expected: string) =
                        match expr with
                        | :? Ast.Expr.Apply as applyExpr ->
                            let hasDirectArg =
                                applyExpr.args
                                |> List.exists (fun arg ->
                                    match arg with
                                    | :? Ast.Expr.String as s -> s.value = expected
                                    | _ -> false)
                            hasDirectArg || containsExpectedStringArg applyExpr.func expected
                        | _ -> false

                    let stmtIsCallWithSingleStringArg (stmt: Ast.Stmt) (expected: string) =
                        match stmt with
                        | :? Ast.Stmt.ExprStmt as exprStmt ->
                            containsExpectedStringArg exprStmt.expr expected
                        | _ -> false

                    Assert.True(
                        stmtIsCallWithSingleStringArg blockExpr.stmts.[0] "hello",
                        "1 番目の文が \"hello\" Console'WriteLine. の Apply として解析されていません")
                    Assert.True(
                        stmtIsCallWithSingleStringArg blockExpr.stmts.[1] "world",
                        "2 番目の文が \"world\" Console'WriteLine. の Apply として解析されていません")
                | _ ->
                    Assert.True(false, "main body was not parsed into a block expression")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses whitespace separated array type application`` () =
        let program = "fn join (xs: Array String): () = ()"

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "join" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.args with
                | [ (:? Ast.FnArg.Named as arg) ] ->
                    match arg.typeExpr with
                    | :? Ast.TypeExpr.Apply as appType ->
                        match appType.head, appType.args with
                        | :? Ast.TypeExpr.Id as headType, [ (:? Ast.TypeExpr.Id as argType) ] ->
                            Assert.Equal("Array", headType.name)
                            Assert.Equal("String", argType.name)
                        | _ ->
                            Assert.True(false, "type application did not preserve Array/String shape")
                    | _ ->
                        Assert.True(false, "argument type was not parsed as Ast.TypeExpr.Apply")
                | _ ->
                    Assert.True(false, "function argument list was not parsed as a single named argument")
            | None ->
                Assert.True(false, "join function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses lambda expression at expr top-level`` () =
        let program = "fn main (): Int = 1 (fn x -> x)."

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Apply as applyExpr ->
                    match applyExpr.func with
                    | :? Ast.Expr.Lambda as lambdaExpr ->
                        Assert.Equal<string list>(["x"], lambdaExpr.args)
                    | _ ->
                        Assert.True(false, "apply target was not parsed as Ast.Expr.Lambda")
                | _ ->
                    Assert.True(false, "main body was not parsed as Ast.Expr.Apply")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses lambda expression with explicit unit argument list`` () =
        let program = "fn main (): Int = (fn () -> 1)."

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Apply as outerApply ->
                    match outerApply.func with
                    | :? Ast.Expr.Lambda as lambdaExpr ->
                        Assert.Empty(lambdaExpr.args)
                    | _ ->
                        Assert.True(false, "outer apply target was not parsed as Ast.Expr.Lambda")
                | _ ->
                    Assert.True(false, "main body was not parsed as Ast.Expr.Apply")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule reports duplicate lambda parameter as Ast.Expr.Error`` () =
        let program = "fn main (): Int = fn x x -> x"

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Error as errExpr ->
                    Assert.Contains("Duplicate lambda parameter", errExpr.message)
                | _ ->
                    Assert.True(false, "duplicate lambda parameter should be reported as Ast.Expr.Error")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule reports missing lambda parameter list as Ast.Expr.Error`` () =
        let program = "fn main (): Int = fn -> 1"

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Error as errExpr ->
                    Assert.Contains("Lambda parameter list is empty", errExpr.message)
                | _ ->
                    Assert.True(false, "missing lambda parameters should be reported as Ast.Expr.Error")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses apostrophe member access with dot call`` () =
        let program = "fn main (): Int = 1 value'transform."

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Apply as applyExpr ->
                    match applyExpr.func with
                    | :? Ast.Expr.MemberAccess as memberAccess ->
                        Assert.Equal("transform", memberAccess.memberName)
                    | _ ->
                        Assert.True(false, "call target was not parsed as Ast.Expr.MemberAccess")
                | _ ->
                    Assert.True(false, "main body was not parsed as Ast.Expr.Apply")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses direct callable zero argument call`` () =
        let program = "fn main (): Int = callable ."

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Apply as applyExpr ->
                    Assert.Empty(applyExpr.args)
                | _ ->
                    Assert.True(false, "main body was not parsed as zero-arg Ast.Expr.Apply")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule reports dangling apostrophe member access as Ast.Expr.Error`` () =
        let program = "fn main (): Int = value'"

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Error as errorExpr ->
                    Assert.Contains("Expected member identifier after apostrophe", errorExpr.message)
                | _ ->
                    Assert.True(false, "dangling apostrophe should be parsed as Ast.Expr.Error")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule rejects missing dot in call chain`` () =
        let program = "fn main (): Int = 1 increment"

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)
            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Error as errorExpr ->
                    Assert.Contains("Expected '.' after callee in call expression.", errorExpr.message)
                | _ ->
                    Assert.True(false, "missing dot in call chain should produce Ast.Expr.Error")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed unexpectedly: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses member access as primary before dot call`` () =
        let program = "fn main (): Int = c a'b."

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Apply as applyExpr ->
                    match applyExpr.func with
                    | :? Ast.Expr.MemberAccess as memberAccess ->
                        match memberAccess.receiver with
                        | :? Ast.Expr.Id as receiverId ->
                            Assert.Equal("a", receiverId.name)
                            Assert.Equal("b", memberAccess.memberName)
                        | _ ->
                            Assert.True(false, "member receiver should be parsed as identifier 'a'")
                    | _ ->
                        Assert.True(false, "dot call target should be parsed as member access")
                | _ ->
                    Assert.True(false, "main body was not parsed as Ast.Expr.Apply")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses member access assignment target`` () =
        let program = """
fn main (): () = do
    window'Width = 320
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    match blockExpr.stmts |> List.tryHead with
                    | Some (:? Ast.Stmt.Assign as assignStmt) ->
                        match assignStmt.target with
                        | :? Ast.Expr.MemberAccess as memberAccess ->
                            match memberAccess.receiver with
                            | :? Ast.Expr.Id as receiverId ->
                                Assert.Equal("window", receiverId.name)
                                Assert.Equal("Width", memberAccess.memberName)
                            | _ ->
                                Assert.True(false, "member assignment receiver should be parsed as Ast.Expr.Id")
                        | _ ->
                            Assert.True(false, "assignment target should be parsed as Ast.Expr.MemberAccess")
                    | _ ->
                        Assert.True(false, "block should contain Ast.Stmt.Assign")
                | _ ->
                    Assert.True(false, "main body was not parsed into block expression")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses chained dot-only calls left to right`` () =
        let program = "fn main (): Int = x f. g."

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Apply as outerApply ->
                    match outerApply.func, outerApply.args with
                    | (:? Ast.Expr.Id as outerFunc), [ (:? Ast.Expr.Apply as innerApply) ] ->
                        Assert.Equal("g", outerFunc.name)
                        match innerApply.func, innerApply.args with
                        | (:? Ast.Expr.Id as innerFunc), [ (:? Ast.Expr.Id as inputExpr) ] ->
                            Assert.Equal("f", innerFunc.name)
                            Assert.Equal("x", inputExpr.name)
                        | _ ->
                            Assert.True(false, "inner chained call should be f(x)")
                    | _ ->
                        Assert.True(false, "outer chained call should be g(f(x))")
                | _ ->
                    Assert.True(false, "main body was not parsed as chained Ast.Expr.Apply")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses multi argument dot-only call`` () =
        let program = "fn main (): Int = a b c sum3."

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Apply as applyExpr ->
                    match applyExpr.func, applyExpr.args with
                    | (:? Ast.Expr.Id as funcId), [ (:? Ast.Expr.Id as arg0); (:? Ast.Expr.Id as arg1); (:? Ast.Expr.Id as arg2) ] ->
                        Assert.Equal("sum3", funcId.name)
                        Assert.Equal("a", arg0.name)
                        Assert.Equal("b", arg1.name)
                        Assert.Equal("c", arg2.name)
                    | _ ->
                        Assert.True(false, "multi-argument dot call should normalize to sum3(a, b, c)")
                | _ ->
                    Assert.True(false, "main body was not parsed as Ast.Expr.Apply")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule rejects missing dot in multi argument call chain`` () =
        let program = "fn main (): Int = a b sum3"

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Error as errorExpr ->
                    Assert.Contains("Expected '.' after callee in call expression.", errorExpr.message)
                | _ ->
                    Assert.True(false, "missing dot in multi-argument call chain should produce Ast.Expr.Error")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed unexpectedly: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses identifier dot as zero argument call`` () =
        let program = "fn main (): Int = ping."

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "main" -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Apply as applyExpr ->
                    match applyExpr.func with
                    | :? Ast.Expr.Id as targetId ->
                        Assert.Equal("ping", targetId.name)
                        Assert.Empty(applyExpr.args)
                    | _ ->
                        Assert.True(false, "zero argument dot call target should be identifier")
                | _ ->
                    Assert.True(false, "main body was not parsed as zero-arg Ast.Expr.Apply")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses impl declaration with explicit this argument`` () =
        let program = """
data Line =
    { slope: Float
    , intercept: Float
    }
impl Line
    fn evaluate (this: Line) (x: Float): Float =
        this'slope * x + this'intercept
"""

        match parseModule program with
        | Success (astModule, _) ->
            let implDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Impl as implDecl -> Some implDecl
                    | _ -> None)
            match implDecl with
            | Some implDecl ->
                Assert.Equal("Line", implDecl.typeName)
                Assert.True(implDecl.forTypeName.IsNone)
                Assert.Single(implDecl.methods) |> ignore
                let methodDecl = implDecl.methods.Head
                Assert.Equal("evaluate", methodDecl.name)
            | None ->
                Assert.True(false, "impl declaration was not parsed")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses impl declaration with subtype for clause`` () =
        let program = """
data B =
    { value: Int }
impl B for A
    fn evaluate (this: B): Int =
        this'value
"""

        match parseModule program with
        | Success (astModule, _) ->
            let implDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Impl as implDecl -> Some implDecl
                    | _ -> None)
            match implDecl with
            | Some parsedImplDecl ->
                Assert.Equal("B", parsedImplDecl.typeName)
                Assert.Equal(Some "A", parsedImplDecl.forTypeName)
                Assert.Single(parsedImplDecl.methods) |> ignore
            | None ->
                Assert.True(false, "impl declaration with for clause was not parsed")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
