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
            Parser.fileModule tokenInput tokens.Head.span.left
        | Failure (reason, span) ->
            Failure ($"Lexing failed: {reason}", span)

    /// 関数本体の終端式を取得する。関数本体が Block の場合は末尾の ExprStmt を返す。
    let private terminalBodyExpr (bodyExpr: Ast.Expr) : Ast.Expr =
        match bodyExpr with
        | :? Ast.Expr.Block as blockExpr ->
            match blockExpr.stmts |> List.tryLast with
            | Some (:? Ast.Stmt.ExprStmt as exprStmt) -> exprStmt.expr
            | _ -> bodyExpr
        | _ -> bodyExpr

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
        let program = """
fn main (): Int =
    if
    | 1 == 1 => 1
    | else => 0
"""

        match parseModule program with
        | Success (astModule, _) ->
            Assert.Single(astModule.decls) |> ignore
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses multi-branch if without error nodes`` () =
        let program = """
fn applyOp (op: String): Int =
    if
    | op == "+" => 1
    | op == "-" => 2
    | op == "*" => 3
    | else => 0
"""

        let rec hasErrorNode (expr: Ast.Expr) =
            match expr with
            | :? Ast.Expr.Error -> true
            | :? Ast.Expr.Block as b ->
                b.stmts |> List.exists (fun stmt ->
                    match stmt with
                    | :? Ast.Stmt.ExprStmt as es -> hasErrorNode es.expr
                    | :? Ast.Stmt.Error -> true
                    | _ -> false)
            | :? Ast.Expr.If as ifExpr ->
                ifExpr.branches |> List.exists (fun branch ->
                    match branch with
                    | :? Ast.IfBranch.Then as tb -> hasErrorNode tb.cond || hasErrorNode tb.body
                    | :? Ast.IfBranch.Else as eb -> hasErrorNode eb.body
                    | _ -> false)
            | _ -> false

        match parseModule program with
        | Success (astModule, _) ->
            let fn =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn -> Some fn
                    | _ -> None)

            match fn with
            | Some fn ->
                Assert.False(hasErrorNode fn.body, "if expression should parse without error nodes")
                let ifExpr =
                    match fn.body with
                    | :? Ast.Expr.Block as b ->
                        b.stmts |> List.tryPick (fun s ->
                            match s with
                            | :? Ast.Stmt.ExprStmt as es ->
                                match es.expr with
                                | :? Ast.Expr.If as ifExpr -> Some ifExpr
                                | _ -> None
                            | _ -> None)
                    | :? Ast.Expr.If as ifExpr -> Some ifExpr
                    | _ -> None

                match ifExpr with
                | Some ifExpr -> Assert.Equal(4, ifExpr.branches.Length)
                | None -> Assert.True(false, "if expression was not found in function body")
            | None ->
                Assert.True(false, "function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses for statement in do block`` () =
        let program = """
fn main: () =
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
                match terminalBodyExpr fn.body with
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

fn main: () =
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
    let ``fileModule parses break statement in for body`` () =
        let program = """
fn main: () =
    for i in values
        if | i == 3 =>
            break
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
                match terminalBodyExpr fn.body with
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
                        let rec containsBreak (s: Ast.Stmt) : bool =
                            match s with
                            | :? Ast.Stmt.Break -> true
                            | :? Ast.Stmt.If as ifStmt ->
                                ifStmt.branches
                                |> List.exists (fun branch ->
                                    match branch with
                                    | :? Ast.IfBranch.Then as t ->
                                        match t.body with
                                        | :? Ast.Expr.Block as b -> b.stmts |> List.exists containsBreak
                                        | _ -> false
                                    | _ -> false)
                            | _ -> false
                        Assert.True(stmt.body |> List.exists containsBreak, "for body does not contain a break statement")
                    | None ->
                        Assert.True(false, "for statement was not parsed into Ast.Stmt.For")
                | _ ->
                    Assert.True(false, "function body was not parsed into a block expression")
            | None ->
                Assert.True(false, "function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses index access expression with bracket syntax`` () =
        let program = """
import System'Console

fn main: () =
    let line = Console'ReadLine.
    let a = " " line'Split.
    a[0] Console'WriteLine.
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

fn main: () =
    let config = AppBuilder'Configure<Application>.
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
    let ``fileModule parses struct declaration with val and var fields`` () =
        let program = """
struct Person
    val name: String
    var age: Int
"""

        match parseModule program with
        | Success (astModule, _) ->
            match astModule.decls with
            | [ (:? Ast.Decl.Data as dataDecl) ] ->
                Assert.Equal("Person", dataDecl.name)
                Assert.Equal(2, dataDecl.items.Length)
                match dataDecl.items with
                | [ (:? Ast.DataItem.Field as nameField); (:? Ast.DataItem.Field as ageField) ] ->
                    Assert.Equal("name", nameField.name)
                    Assert.False(nameField.isMutable)
                    Assert.Equal("age", ageField.name)
                    Assert.True(ageField.isMutable)
                | _ ->
                    Assert.True(false, "expected two Ast.DataItem.Field items")
            | _ ->
                Assert.True(false, "expected a single Ast.Decl.Data declaration")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule rejects struct field without val or var`` () =
        let program = """
struct Person
    name: String
"""

        match parseModule program with
        | Success (astModule, _) ->
            // パースは成功しても、val/var 欠落により Decl.Error が混入することを許容する。
            // 少なくとも正常な Decl.Data として登録されないことを確認する。
            let hasValidStruct =
                astModule.decls
                |> List.exists (fun decl ->
                    match decl with
                    | :? Ast.Decl.Data as dd -> not dd.items.IsEmpty
                    | _ -> false)
            Assert.False(hasValidStruct, "expected val/var to be required on struct fields")
        | Failure _ -> ()

    [<Fact>]
    let ``fileModule parses struct constructor via record literal applied to type`` () =
        let program = """
struct Person
    val name: String
    val age: Int
fn main (): Person = { name = "Alice", age = 20 } Person.
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
                match terminalBodyExpr fnDecl.body with
                | :? Ast.Expr.Apply as applyExpr ->
                    match applyExpr.func with
                    | :? Ast.Expr.Id as typeId -> Assert.Equal("Person", typeId.name)
                    | _ -> Assert.True(false, "expected callee to be type identifier")
                    match applyExpr.args with
                    | [ (:? Ast.Expr.RecordLit as recordLit) ] ->
                        Assert.Equal(2, recordLit.fields.Length)
                    | _ ->
                        Assert.True(false, "expected single RecordLit argument")
                | _ ->
                    Assert.True(false, "expected Ast.Expr.Apply in main body")
            | None ->
                Assert.True(false, "main declaration not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses enum declaration`` () =
        let program = """
enum Color
    | Black
    | Rgb { r: Int, g: Int, b: Int }
"""

        match parseModule program with
        | Success (astModule, _) ->
            let enumDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Enum as enumDecl -> Some enumDecl
                    | _ -> None)

            match enumDecl with
            | Some enumDecl ->
                Assert.Equal("Color", enumDecl.name)
                Assert.Equal(2, enumDecl.cases.Length)
                match enumDecl.cases |> List.last with
                | :? Ast.EnumCase.Case as caseDecl ->
                    Assert.Equal("Rgb", caseDecl.name)
                    Assert.Equal(3, caseDecl.fields.Length)
                | _ ->
                    Assert.True(false, "expected Ast.EnumCase.Case for enum case")
            | None ->
                Assert.True(false, "enum declaration not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses enum initialization expression with payload`` () =
        let program = """
enum Color
    | Rgb { r: Int, g: Int, b: Int }

fn main (): Color = Color'Rgb { r = 255, g = 0, b = 0 }
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
                match terminalBodyExpr fnDecl.body with
                | :? Ast.Expr.EnumInit as initExpr ->
                    Assert.Equal("Color", initExpr.typeName)
                    Assert.Equal("Rgb", initExpr.caseName)
                    Assert.Equal(3, initExpr.fields.Length)
                | _ ->
                    Assert.True(false, "expected Ast.Expr.EnumInit in main body")
            | None ->
                Assert.True(false, "main declaration not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses match expression with enum patterns`` () =
        let program = """
fn main (color: Color): Int =
    match color
    | Color'Black -> 0
    | Color'Rgb { r, .. } -> r
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
                match terminalBodyExpr fnDecl.body with
                | :? Ast.Expr.Match as matchExpr ->
                    Assert.Equal(2, matchExpr.arms.Length)
                    match matchExpr.arms |> List.last with
                    | :? Ast.MatchArm.Arm as arm ->
                        match arm.pattern with
                        | :? Ast.Pattern.Enum as enumPattern ->
                            Assert.Equal("Color", enumPattern.typeName)
                            Assert.Equal("Rgb", enumPattern.caseName)
                            Assert.True(enumPattern.hasRest)
                            Assert.Single(enumPattern.fields) |> ignore
                        | _ ->
                            Assert.True(false, "expected Ast.Pattern.Enum in match arm")
                    | _ ->
                        Assert.True(false, "expected Ast.MatchArm.Arm in match expression")
                | _ ->
                    Assert.True(false, "expected Ast.Expr.Match in main body")
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
    let ``fileModule parses unary minus float literal in struct initialization`` () =
        let program = """
struct Line
    val slope: Float
    val intercept: Float
fn main (): Line = { slope = 2.0, intercept = -1.0 } Line.
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
                match terminalBodyExpr fnDecl.body with
                | :? Ast.Expr.Apply as applyExpr ->
                    match applyExpr.args with
                    | [ (:? Ast.Expr.RecordLit as recordLit) ] ->
                        match recordLit.fields |> List.tryFind (fun field -> match field with | :? Ast.DataInitField.Field as f -> f.name = "intercept" | _ -> false) with
                        | Some (:? Ast.DataInitField.Field as interceptField) ->
                            match interceptField.value with
                            | :? Ast.Expr.Double as doubleExpr ->
                                Assert.Equal(-1.0, doubleExpr.value)
                            | _ ->
                                Assert.True(false, "intercept field was not parsed as Ast.Expr.Double")
                        | _ ->
                            Assert.True(false, "intercept field initializer was not found")
                    | _ ->
                        Assert.True(false, "expected single RecordLit argument to Line constructor")
                | _ ->
                    Assert.True(false, "main body was not parsed as Ast.Expr.Apply")
            | None ->
                Assert.True(false, "main function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses consecutive exprStmts in do block as separate statements`` () =
        // 回帰テスト: 同じインデントレベルの連続する exprStmt が 1 つの Apply 式として誤解析されないことを確認する。
        let program = """
import System'Console

fn main (): () =
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
                match terminalBodyExpr fn.body with
                | :? Ast.Expr.Apply as applyExpr ->
                    match applyExpr.func with
                    | :? Ast.Expr.Lambda as lambdaExpr ->
                        Assert.Equal(1, lambdaExpr.args.Length)
                        match lambdaExpr.args.[0] with
                        | :? Ast.FnArg.Inferred as inferredArg ->
                            Assert.Equal("x", inferredArg.name)
                        | _ ->
                            Assert.True(false, "lambda arg was not parsed as Ast.FnArg.Inferred")
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
                match terminalBodyExpr fn.body with
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
                match terminalBodyExpr fn.body with
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
                match terminalBodyExpr fn.body with
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
                match terminalBodyExpr fn.body with
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
                match terminalBodyExpr fn.body with
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
                match terminalBodyExpr fn.body with
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
                match terminalBodyExpr fn.body with
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
                match terminalBodyExpr fn.body with
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
fn main (): () =
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
                match terminalBodyExpr fn.body with
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
                match terminalBodyExpr fn.body with
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
                match terminalBodyExpr fn.body with
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
                match terminalBodyExpr fn.body with
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
                match terminalBodyExpr fn.body with
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
    let ``fileModule parses impl declaration with self receiver argument`` () =
        let program = """
struct Line
    val slope: Float
    val intercept: Float
impl Line
    fn evaluate self (x: Float): Float =
        self'slope * x + self'intercept
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
struct B
    val value: Int
impl B for A
    fn evaluate self: Int =
        self'value
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

    [<Fact>]
    let ``fileModule parses impl declaration with subtype for and by clause`` () =
        let program = """
struct Wrapper
    val value: Int
    val inner: Base
impl Base for Wrapper by inner
"""

        match parseModule program with
        | Success (astModule, _) ->
            let implDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Impl as parsedImplDecl -> Some parsedImplDecl
                    | _ -> None)

            match implDecl with
            | Some parsedImplDecl ->
                Assert.Equal("Base", parsedImplDecl.typeName)
                Assert.True(parsedImplDecl.asTypeName.IsNone)
                Assert.Equal(Some "Wrapper", parsedImplDecl.forTypeName)
                Assert.Equal(Some "inner", parsedImplDecl.byFieldName)
                Assert.Empty(parsedImplDecl.methods)
            | None ->
                Assert.True(false, "impl declaration with for/by clause was not parsed")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses impl declaration with as clause`` () =
        let program = """
struct MyButton
    val label: String
impl MyButton as Button
    fn click self: Unit = ()
"""

        match parseModule program with
        | Success (astModule, _) ->
            let implDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Impl as parsedImplDecl -> Some parsedImplDecl
                    | _ -> None)

            match implDecl with
            | Some parsedImplDecl ->
                Assert.Equal("MyButton", parsedImplDecl.typeName)
                Assert.Equal(Some "Button", parsedImplDecl.asTypeName)
                Assert.True(parsedImplDecl.forTypeName.IsNone)
                Assert.True(parsedImplDecl.byFieldName.IsNone)
                Assert.Single(parsedImplDecl.methods) |> ignore
            | None ->
                Assert.True(false, "impl declaration with as clause was not parsed")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses impl as clause with no methods`` () =
        let program = """
struct Widget
    val id: Int
impl Widget as Control
"""

        match parseModule program with
        | Success (astModule, _) ->
            let implDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Impl as parsedImplDecl -> Some parsedImplDecl
                    | _ -> None)

            match implDecl with
            | Some parsedImplDecl ->
                Assert.Equal("Widget", parsedImplDecl.typeName)
                Assert.Equal(Some "Control", parsedImplDecl.asTypeName)
                Assert.True(parsedImplDecl.forTypeName.IsNone)
                Assert.True(parsedImplDecl.byFieldName.IsNone)
                Assert.Empty(parsedImplDecl.methods)
            | None ->
                Assert.True(false, "impl as declaration without methods was not parsed")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses override fn in impl as block`` () =
        let program = """
struct MyButton
    val label: String
impl MyButton as Button
    override fn click self: Unit = ()
    fn helper self: Unit = ()
"""

        match parseModule program with
        | Success (astModule, _) ->
            let implDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Impl as parsedImplDecl -> Some parsedImplDecl
                    | _ -> None)

            match implDecl with
            | Some parsedImplDecl ->
                Assert.Equal("MyButton", parsedImplDecl.typeName)
                Assert.Equal(Some "Button", parsedImplDecl.asTypeName)
                Assert.Equal(2, parsedImplDecl.methods.Length)
                let clickMethod = parsedImplDecl.methods |> List.find (fun m -> m.name = "click")
                let helperMethod = parsedImplDecl.methods |> List.find (fun m -> m.name = "helper")
                Assert.True(clickMethod.isOverride, "click method should have isOverride = true")
                Assert.False(helperMethod.isOverride, "helper method should have isOverride = false")
            | None ->
                Assert.True(false, "impl declaration was not parsed")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses override fn even in plain impl block (resolve will reject later)`` () =
        // override 修飾子は構文上はどの impl ブロックでも受理する。
        // `impl A as B` 以外でのエラーは Resolve フェーズで報告される。
        let program = """
struct Foo
    val x: Int
impl Foo
    override fn bar self: Unit = ()
"""

        match parseModule program with
        | Success (astModule, _) ->
            let implDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Impl as parsedImplDecl -> Some parsedImplDecl
                    | _ -> None)

            match implDecl with
            | Some parsedImplDecl ->
                Assert.True(parsedImplDecl.asTypeName.IsNone)
                Assert.Single(parsedImplDecl.methods) |> ignore
                let barMethod = parsedImplDecl.methods.Head
                Assert.True(barMethod.isOverride, "override should be captured even in plain impl block")
            | None ->
                Assert.True(false, "impl declaration was not parsed")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule top-level fn does not accept override modifier`` () =
        // トップレベル fn の前に override を書くのは構文エラー。
        // fnDecl パーサが override を受理しないため、Decl.Error として吸収される。
        let program = """
override fn main (): Unit = ()
"""

        match parseModule program with
        | Success (astModule, _) ->
            let hasFn =
                astModule.decls
                |> List.exists (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fnDecl -> fnDecl.name = "main"
                    | _ -> false)
            Assert.False(hasFn, "top-level fn with 'override' modifier should not be parsed as a valid Fn declaration")
        | Failure _ ->
            // パース失敗も期待される結果。
            ()

    [<Fact>]
    let ``fileModule parses async fn in impl as block`` () =
        let program = """
struct MyTask
    val value: Int
impl MyTask as Object
    async fn run self: Unit = ()
    fn sync self: Unit = ()
"""

        match parseModule program with
        | Success (astModule, _) ->
            let implDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Impl as parsedImplDecl -> Some parsedImplDecl
                    | _ -> None)

            match implDecl with
            | Some parsedImplDecl ->
                Assert.Equal(2, parsedImplDecl.methods.Length)
                let runMethod = parsedImplDecl.methods |> List.find (fun m -> m.name = "run")
                let syncMethod = parsedImplDecl.methods |> List.find (fun m -> m.name = "sync")
                Assert.True(runMethod.isAsync, "run method should have isAsync = true")
                Assert.False(runMethod.isOverride, "run method should not have isOverride")
                Assert.False(syncMethod.isAsync, "sync method should have isAsync = false")
            | None ->
                Assert.True(false, "impl declaration was not parsed")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses override async fn in impl as block`` () =
        let program = """
struct MyTask
    val value: Int
impl MyTask as Object
    override async fn run self: Unit = ()
"""

        match parseModule program with
        | Success (astModule, _) ->
            let implDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Impl as parsedImplDecl -> Some parsedImplDecl
                    | _ -> None)

            match implDecl with
            | Some parsedImplDecl ->
                Assert.Single(parsedImplDecl.methods) |> ignore
                let runMethod = parsedImplDecl.methods.Head
                Assert.True(runMethod.isOverride, "run method should have isOverride = true")
                Assert.True(runMethod.isAsync, "run method should have isAsync = true")
            | None ->
                Assert.True(false, "impl declaration was not parsed")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses async fn at top level`` () =
        let program = "async fn run (): Unit = ()"

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn -> Some fn
                    | _ -> None)
            match fnDecl with
            | Some fn ->
                Assert.True(fn.isAsync, "top-level fn with 'async' should have isAsync = true")
                Assert.False(fn.isOverride, "top-level async fn should have isOverride = false")
            | None ->
                Assert.True(false, "no fn declaration found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses await expr in fn body`` () =
        let program = "async fn run (x: Int): Unit = await x"

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn -> Some fn
                    | _ -> None)
            match fnDecl with
            | Some fn ->
                Assert.True(fn.isAsync, "fn should be async")
                match terminalBodyExpr fn.body with
                | :? Ast.Expr.Await as awaitExpr ->
                    match awaitExpr.operand with
                    | :? Ast.Expr.Id as idExpr ->
                        Assert.Equal("x", idExpr.name)
                    | other ->
                        Assert.True(false, $"expected Ast.Expr.Id but got {other.GetType().Name}")
                | other ->
                    Assert.True(false, $"expected Ast.Expr.Await but got {other.GetType().Name}")
            | None ->
                Assert.True(false, "no fn declaration found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses await with dot-call operand`` () =
        // `await x f.` は `await (f(x))` として解析され、operand は Apply となる。
        let program = "async fn run (x: Int): Unit = await x f."

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn -> Some fn
                    | _ -> None)
            match fnDecl with
            | Some fn ->
                match terminalBodyExpr fn.body with
                | :? Ast.Expr.Await as awaitExpr ->
                    match awaitExpr.operand with
                    | :? Ast.Expr.Apply -> ()
                    | other ->
                        Assert.True(false, $"expected Apply operand but got {other.GetType().Name}")
                | other ->
                    Assert.True(false, $"expected Ast.Expr.Await but got {other.GetType().Name}")
            | None ->
                Assert.True(false, "no fn declaration found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses true literal as Bool`` () =
        let program = "fn main (): Bool = True"

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn -> Some fn
                    | _ -> None)
            match fnDecl with
            | Some fn ->
                match terminalBodyExpr fn.body with
                | :? Ast.Expr.Bool as boolExpr ->
                    Assert.True(boolExpr.value, "expected true literal")
                | other ->
                    Assert.True(false, $"expected Ast.Expr.Bool but got {other.GetType().Name}")
            | None ->
                Assert.True(false, "no fn declaration found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses false literal as Bool`` () =
        let program = "fn main (): Bool = False"

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn -> Some fn
                    | _ -> None)
            match fnDecl with
            | Some fn ->
                match terminalBodyExpr fn.body with
                | :? Ast.Expr.Bool as boolExpr ->
                    Assert.False(boolExpr.value, "expected false literal")
                | other ->
                    Assert.True(false, $"expected Ast.Expr.Bool but got {other.GetType().Name}")
            | None ->
                Assert.True(false, "no fn declaration found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses compound add assignment`` () =
        let program = """
fn main (): Unit =
    var x = 1
    x += 2
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl -> match decl with | :? Ast.Decl.Fn as fn -> Some fn | _ -> None)
            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    match blockExpr.stmts |> List.tryLast with
                    | Some (:? Ast.Stmt.CompoundAssign as compoundAssignStmt) ->
                        Assert.Equal(Ast.Stmt.CompoundAssignOp.Add, compoundAssignStmt.op)
                    | _ -> Assert.True(false, "expected compound assignment statement")
                | _ -> Assert.True(false, "expected block body")
            | None -> Assert.True(false, "no fn declaration found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses compound subtract assignment`` () =
        let program = """
fn main (): Unit =
    var x = 3
    x -= 1
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl -> match decl with | :? Ast.Decl.Fn as fn -> Some fn | _ -> None)
            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    match blockExpr.stmts |> List.tryLast with
                    | Some (:? Ast.Stmt.CompoundAssign as compoundAssignStmt) ->
                        Assert.Equal(Ast.Stmt.CompoundAssignOp.Sub, compoundAssignStmt.op)
                    | _ -> Assert.True(false, "expected compound assignment statement")
                | _ -> Assert.True(false, "expected block body")
            | None -> Assert.True(false, "no fn declaration found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses compound multiply assignment`` () =
        let program = """
fn main (): Unit =
    var x = 3
    x *= 2
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl -> match decl with | :? Ast.Decl.Fn as fn -> Some fn | _ -> None)
            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    match blockExpr.stmts |> List.tryLast with
                    | Some (:? Ast.Stmt.CompoundAssign as compoundAssignStmt) ->
                        Assert.Equal(Ast.Stmt.CompoundAssignOp.Mul, compoundAssignStmt.op)
                    | _ -> Assert.True(false, "expected compound assignment statement")
                | _ -> Assert.True(false, "expected block body")
            | None -> Assert.True(false, "no fn declaration found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses compound divide assignment`` () =
        let program = """
fn main (): Unit =
    var x = 10
    x /= 2
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl -> match decl with | :? Ast.Decl.Fn as fn -> Some fn | _ -> None)
            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    match blockExpr.stmts |> List.tryLast with
                    | Some (:? Ast.Stmt.CompoundAssign as compoundAssignStmt) ->
                        Assert.Equal(Ast.Stmt.CompoundAssignOp.Div, compoundAssignStmt.op)
                    | _ -> Assert.True(false, "expected compound assignment statement")
                | _ -> Assert.True(false, "expected block body")
            | None -> Assert.True(false, "no fn declaration found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    let private firstLetStmt (astModule: Ast.Module) (name: string) : Ast.Stmt.Let option =
        astModule.decls
        |> List.tryPick (fun decl -> match decl with | :? Ast.Decl.Fn as fn -> Some fn | _ -> None)
        |> Option.bind (fun fn ->
            match fn.body with
            | :? Ast.Expr.Block as blockExpr ->
                blockExpr.stmts
                |> List.tryPick (fun stmt -> match stmt with | :? Ast.Stmt.Let as letStmt when letStmt.name = name -> Some letStmt | _ -> None)
            | _ -> None)

    [<Fact>]
    let ``fileModule parses let binding type annotation`` () =
        let program = """
fn main: () =
    let x: List Int = List.
    x
"""

        match parseModule program with
        | Success (astModule, _) ->
            match firstLetStmt astModule "x" with
            | Some letStmt ->
                match letStmt.typeAnnotation with
                | Some (:? Ast.TypeExpr.Apply as applyType) ->
                    match applyType.head with
                    | :? Ast.TypeExpr.Id as headId -> Assert.Equal("List", headId.name)
                    | _ -> Assert.True(false, "expected List head in type annotation")
                    match applyType.args with
                    | [ (:? Ast.TypeExpr.Id as argId) ] -> Assert.Equal("Int", argId.name)
                    | _ -> Assert.True(false, "expected single Int type argument")
                | _ -> Assert.True(false, "expected Some Apply type annotation on let binding")
            | None -> Assert.True(false, "let binding 'x' was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses var binding type annotation`` () =
        let program = """
fn main: () =
    var x: Int = 0
    x
"""

        match parseModule program with
        | Success (astModule, _) ->
            let varStmt =
                astModule.decls
                |> List.tryPick (fun decl -> match decl with | :? Ast.Decl.Fn as fn -> Some fn | _ -> None)
                |> Option.bind (fun fn ->
                    match fn.body with
                    | :? Ast.Expr.Block as blockExpr ->
                        blockExpr.stmts |> List.tryPick (fun stmt -> match stmt with | :? Ast.Stmt.Var as v -> Some v | _ -> None)
                    | _ -> None)
            match varStmt with
            | Some v ->
                match v.typeAnnotation with
                | Some (:? Ast.TypeExpr.Id as idType) -> Assert.Equal("Int", idType.name)
                | _ -> Assert.True(false, "expected Some Int type annotation on var binding")
            | None -> Assert.True(false, "var binding 'x' was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses expression type ascription`` () =
        let program = """
fn main: () =
    let y = List. : List Int
    y
"""

        match parseModule program with
        | Success (astModule, _) ->
            match firstLetStmt astModule "y" with
            | Some letStmt ->
                Assert.True(letStmt.typeAnnotation.IsNone, "let binding 'y' should have no binding annotation")
                match letStmt.value with
                | :? Ast.Expr.TypeAscription as asc ->
                    match asc.typeExpr with
                    | :? Ast.TypeExpr.Apply as applyType ->
                        match applyType.head with
                        | :? Ast.TypeExpr.Id as headId -> Assert.Equal("List", headId.name)
                        | _ -> Assert.True(false, "expected List head in ascription type")
                    | _ -> Assert.True(false, "expected Apply ascription type")
                | _ -> Assert.True(false, "expected let value to be a TypeAscription")
            | None -> Assert.True(false, "let binding 'y' was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses generic enum declaration with one type parameter`` () =
        let program = """
enum Opt T
    | None
    | Some { value: T }
"""

        match parseModule program with
        | Success (astModule, _) ->
            let enumDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Enum as enumDecl -> Some enumDecl
                    | _ -> None)
            match enumDecl with
            | Some enumDecl ->
                Assert.Equal("Opt", enumDecl.name)
                Assert.Equal<string list>(["T"], enumDecl.typeParams)
                Assert.Equal(2, enumDecl.cases.Length)
                match enumDecl.cases |> List.item 1 with
                | :? Ast.EnumCase.Case as someCase ->
                    Assert.Equal("Some", someCase.name)
                    Assert.Equal(1, someCase.fields.Length)
                | _ -> Assert.True(false, "expected Ast.EnumCase.Case for Some case")
            | None ->
                Assert.True(false, "enum declaration not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses generic impl declaration with type parameter`` () =
        let program = """
enum Opt T
    | None
    | Some { value: T }

impl Opt T
    fn isSome self: Bool = true
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
                Assert.Equal("Opt", implDecl.typeName)
                Assert.Equal<string list>(["T"], implDecl.typeParams)
                Assert.Equal(1, implDecl.methods.Length)
                Assert.Equal("isSome", implDecl.methods.[0].name)
            | None ->
                Assert.True(false, "impl declaration not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses if statement without else`` () =
        let program = """
fn main (): Int =
    if | True =>
        return 1
    2
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
                    let ifStmt =
                        blockExpr.stmts
                        |> List.tryPick (fun stmt ->
                            match stmt with
                            | :? Ast.Stmt.If as ifStmt -> Some ifStmt
                            | _ -> None)
                    match ifStmt with
                    | Some stmt ->
                        Assert.Equal(1, stmt.branches.Length)
                        Assert.True(stmt.branches.[0] :? Ast.IfBranch.Then, "branch should be Then")
                    | None ->
                        Assert.True(false, "if statement was not parsed into Ast.Stmt.If")
                | _ ->
                    Assert.True(false, "function body was not parsed into a block expression")
            | None ->
                Assert.True(false, "function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses if-else statement`` () =
        // else あり `if` ブロックは exprStmt (ifExpr) として解析される
        let program = """
fn main (): Int =
    if
    | True =>
        return 1
    | else =>
        return 0
    2
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
                    let ifExpr =
                        blockExpr.stmts
                        |> List.tryPick (fun stmt ->
                            match stmt with
                            | :? Ast.Stmt.ExprStmt as es ->
                                match es.expr with
                                | :? Ast.Expr.If as ifExpr -> Some ifExpr
                                | _ -> None
                            | _ -> None)
                    match ifExpr with
                    | Some ifExpr ->
                        Assert.Equal(2, ifExpr.branches.Length)
                        Assert.True(ifExpr.branches.[0] :? Ast.IfBranch.Then, "first branch should be Then")
                        Assert.True(ifExpr.branches.[1] :? Ast.IfBranch.Else, "second branch should be Else")
                    | None ->
                        Assert.True(false, "if-else expression was not found in function body")
                | _ ->
                    Assert.True(false, "function body was not parsed into a block expression")
            | None ->
                Assert.True(false, "function declaration was not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses let-else statement with positional binding`` () =
        let program = """
fn unwrap (o: Opt Int): Int =
    let Opt'Some x = o
    | else -> return -1
    x
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "unwrap" -> Some fn
                    | _ -> None)
            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    let letElseStmt =
                        blockExpr.stmts
                        |> List.tryPick (fun stmt ->
                            match stmt with
                            | :? Ast.Stmt.LetElse as s -> Some s
                            | _ -> None)
                    match letElseStmt with
                    | Some s ->
                        match s.pattern with
                        | :? Ast.Pattern.Enum as p ->
                            Assert.Equal("Opt", p.typeName)
                            Assert.Equal("Some", p.caseName)
                            Assert.Equal(1, p.fields.Length)
                            Assert.True(p.fields.[0] :? Ast.PatternField.Positional)
                        | _ -> Assert.True(false, "pattern should be Enum")
                        Assert.Equal(1, s.elseBranch.Length)
                    | None ->
                        Assert.True(false, "let-else statement was not found")
                | _ ->
                    Assert.True(false, "function body was not a block")
            | None ->
                Assert.True(false, "function 'unwrap' not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses var-else statement with named field binding`` () =
        let program = """
fn test (o: Opt Int): Int =
    var Opt'Some { value } = o
    | else -> return 0
    value
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "test" -> Some fn
                    | _ -> None)
            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    let varElseStmt =
                        blockExpr.stmts
                        |> List.tryPick (fun stmt ->
                            match stmt with
                            | :? Ast.Stmt.VarElse as s -> Some s
                            | _ -> None)
                    match varElseStmt with
                    | Some s ->
                        match s.pattern with
                        | :? Ast.Pattern.Enum as p ->
                            Assert.Equal("Opt", p.typeName)
                            Assert.Equal("Some", p.caseName)
                        | _ -> Assert.True(false, "pattern should be Enum")
                    | None ->
                        Assert.True(false, "var-else statement was not found")
                | _ ->
                    Assert.True(false, "function body was not a block")
            | None ->
                Assert.True(false, "function 'test' not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses bare return as return unit`` () =
        let program = """
fn doWork (): Unit =
    return
    42 print.
"""
        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn when fn.name = "doWork" -> Some fn
                    | _ -> None)
            match fnDecl with
            | Some fn ->
                match fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    let returnStmt =
                        blockExpr.stmts
                        |> List.tryPick (fun stmt ->
                            match stmt with
                            | :? Ast.Stmt.Return as r -> Some r
                            | _ -> None)
                    match returnStmt with
                    | Some r ->
                        Assert.True(r.expr :? Ast.Expr.Unit, "bare return should have Unit expr")
                    | None ->
                        Assert.True(false, "return statement not found")
                | _ ->
                    Assert.True(false, "function body was not a block")
            | None ->
                Assert.True(false, "function 'doWork' not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses while statement`` () =
        let program = """
fn main: () =
    var x = 0
    while x < 10
        x = x + 1
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match terminalBodyExpr fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    let whileStmt =
                        blockExpr.stmts
                        |> List.tryPick (fun stmt ->
                            match stmt with
                            | :? Ast.Stmt.While as ws -> Some ws
                            | _ -> None)

                    match whileStmt with
                    | Some ws ->
                        Assert.Single(ws.body) |> ignore
                    | None ->
                        Assert.True(false, "while statement was not parsed into Ast.Stmt.While")
                | _ ->
                    Assert.True(false, "function body was not a block")
            | None ->
                Assert.True(false, "function 'main' not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses break and continue in while body`` () =
        let program = """
fn main: () =
    var x = 0
    while x < 100
        if | x == 3 =>
            break
        if | x == 1 =>
            continue
        x = x + 1
"""

        match parseModule program with
        | Success (astModule, _) ->
            let fnDecl =
                astModule.decls
                |> List.tryPick (fun decl ->
                    match decl with
                    | :? Ast.Decl.Fn as fn -> Some fn
                    | _ -> None)

            match fnDecl with
            | Some fn ->
                match terminalBodyExpr fn.body with
                | :? Ast.Expr.Block as blockExpr ->
                    let whileStmt =
                        blockExpr.stmts
                        |> List.tryPick (fun stmt ->
                            match stmt with
                            | :? Ast.Stmt.While as ws -> Some ws
                            | _ -> None)

                    match whileStmt with
                    | Some ws ->
                        let rec containsBreak (stmts: Ast.Stmt list) : bool =
                            stmts |> List.exists (fun s ->
                                match s with
                                | :? Ast.Stmt.Break -> true
                                | :? Ast.Stmt.If as ifS ->
                                    ifS.branches |> List.exists (fun b ->
                                        match b with
                                        | :? Ast.IfBranch.Then as t ->
                                            match t.body with
                                            | :? Ast.Expr.Block as blk -> containsBreak blk.stmts
                                            | _ -> false
                                        | _ -> false)
                                | _ -> false)
                        let rec containsContinue (stmts: Ast.Stmt list) : bool =
                            stmts |> List.exists (fun s ->
                                match s with
                                | :? Ast.Stmt.Continue -> true
                                | :? Ast.Stmt.If as ifS ->
                                    ifS.branches |> List.exists (fun b ->
                                        match b with
                                        | :? Ast.IfBranch.Then as t ->
                                            match t.body with
                                            | :? Ast.Expr.Block as blk -> containsContinue blk.stmts
                                            | _ -> false
                                        | _ -> false)
                                | _ -> false)
                        Assert.True(containsBreak ws.body, "while body should contain break")
                        Assert.True(containsContinue ws.body, "while body should contain continue")
                    | None ->
                        Assert.True(false, "while statement was not parsed into Ast.Stmt.While")
                | _ ->
                    Assert.True(false, "function body was not a block")
            | None ->
                Assert.True(false, "function 'main' not found")
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")
