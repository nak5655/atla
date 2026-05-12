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
        let program = "fn main (): Int = if | 1 == 1 => 1 | else => 0"

        match parseModule program with
        | Success (astModule, _) ->
            Assert.Single(astModule.decls) |> ignore
        | Failure (reason, span) ->
            Assert.True(false, $"Parsing failed: {reason} at {span.left.Line}:{span.left.Column}")

    [<Fact>]
    let ``fileModule parses inline multi-branch if without error nodes`` () =
        // インライン形式（`if  | cond => body` が同一行に `if` と `|` を含む）で
        // ブランチが 3 つ以上あるとき "unexpected input" エラーが発生しないリグレッションテスト。
        //
        // 構造的原因: block コンビネーターが「行の最左トークン（`if`）」の列をオフサイド基準に
        // していたため、ifThen/ifElse のボディスコープが `|` の列まで及ばず、後続ブランチの
        // `|` が BlockInput から不可視にならなかった。
        // 修正: blockAtOpener は opener トークン自身の列をオフサイド基準とする。これにより
        // ifThen ボディ内で後続行の同列 `|` が隠れ、infixOp が届かなくなる。
        //
        // 安全網: `|` は二項演算子ではないため infixOp からも除外している
        // （全ブランチが同一行に並ぶ場合は BlockInput が同行を隠せないため必要）。
        let program = """
fn applyOp (op: String): Int =
    if  | op == "+" => 1
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
    let ``fileModule parses index access expression with bang-bang operator`` () =
        let program = """
import System'Console

fn main: () =
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

fn main: () =
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
                match terminalBodyExpr fnDecl.body with
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
                match terminalBodyExpr fnDecl.body with
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
    let ``fileModule parses impl declaration with explicit this argument`` () =
        let program = """
data Line =
    { slope: Float
    , intercept: Float
    }
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
data B =
    { value: Int }
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
data Wrapper =
    { value: Int
    , inner: Base
    }
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
data MyButton =
    { label: String
    }
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
data Widget =
    { id: Int
    }
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
