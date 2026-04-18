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
import System.Linq.Enumerable

fn main: () = do
    for i in Enumerable.Range 1 3
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
import System.Array
import System.Console
import System.Linq.Enumerable
fn fizzbuzz (n: Int): () =
    for i in Enumerable.Range 1 n
        Console.WriteLine if
            | i % 15 == 0 => "FizzBuzz"
            | i % 5 == 0 => "Buzz"
            | i % 3 == 0 => "Fizz"
            | else => i.ToString()

fn main: () = do
    let n = Int32.Parse (Console.ReadLine ())
    fizzbuzz n
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
import System.Console

fn main: () = do
    let a = (Console.ReadLine ()).Split " "
    Console.WriteLine a !! 0
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

                        if containsIndexAccess exprStmt.expr then
                            Assert.True(true)
                        else
                            Assert.True(false, "index access was not found in parsed expression tree")
                    | _ -> Assert.True(false, "block does not end with expression statement")
                | _ -> Assert.True(false, "main body was not parsed into block expression")
            | None -> Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses generic apply postfix`` () =
        let program = """
import Avalonia.Controls.AppBuilder
import Avalonia.Application

fn main: () = do
    let config = AppBuilder.Configure[Application] ()
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
