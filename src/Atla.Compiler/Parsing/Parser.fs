namespace Atla.Compiler.Parsing

open Atla.Compiler.Ast
open Atla.Compiler.Types
open Atla.Compiler.Parsing.Combinators

type TokenInput (tokens: Token list) =
    let binarySearch pos =
        if (tokens.Length < 2) then
            None
        else
            let rec go lo hi =
                if lo > hi then None
                else
                    let mid = (lo + hi) / 2
                    let midPos = tokens.[mid].span.left
                    if midPos = pos then Some mid
                    elif midPos < pos then go (mid + 1) hi
                    else go lo (mid - 1)
            go 0 (tokens.Length - 1)

    interface Input<Token> with
        member _.get (arg: Position): Token option =
            binarySearch arg |> Option.map (fun i -> tokens.[i])

        member _.next (arg: Position): Position =
            match binarySearch arg with
            | Some i when i < tokens.Length - 1 -> tokens.[i + 1].span.left
            | _ -> {  Line = arg.Line; Column = arg.Column + 1 } // ここでは単純に次の位置を返す。

type BlockInput (tokenInput: Input<Token>, offsideLine: Position) =
    interface Input<Token> with
        member _.get (arg: Position): Token option =
            if arg.Line > offsideLine.Line && arg.Column <= offsideLine.Column then
                None
            else
                tokenInput.get arg

        member _.next (arg: Position): Position =
            if arg.Line > offsideLine.Line && arg.Column <= offsideLine.Column then
                {  Line = arg.Line; Column = arg.Column + 1 } // ここでは単純に次の位置を返す。
            else
                tokenInput.next arg

module Parser =
    let asToken<'T when 'T :> Token> (p: PackratParser<Token, 'T>) : PackratParser<Token, Token> =
        p |>> fun t -> t :> Token
    let asExpr<'E when 'E :> Ast.Expr> (p: PackratParser<Token, 'E>) : PackratParser<Token, Ast.Expr> =
        p |>> fun e -> e :> Ast.Expr
    let asStmt<'S when 'S :> Ast.Stmt> (p: PackratParser<Token, 'S>) : PackratParser<Token, Ast.Stmt> =
        p |>> fun s -> s :> Ast.Stmt
    let asDataItem<'DI when 'DI :> Ast.DataItem> (p: PackratParser<Token, 'DI>) : PackratParser<Token, Ast.DataItem> =
        p |>> fun di -> di :> Ast.DataItem
    let asDecl<'D when 'D :> Ast.Decl> (p: PackratParser<Token, 'D>) : PackratParser<Token, Ast.Decl> =
        p |>> fun d -> d :> Ast.Decl

    let block<'A> (opener: PackratParser<Token, Token>) (body: PackratParser<Token, 'A>): PackratParser<Token, 'A> =
        Delay (fun () -> fun input pos ->
            match opener input pos with
                | Success (token, nextPos) -> 
                    // 先頭のトークンを探す
                    let offsideLine = [| 0 .. token.span.left.Column |] |> Array.find (fun i -> (input.get { Line = token.span.left.Line; Column = i }).IsSome)
                    body (BlockInput (input, { Line = token.span.left.Line; Column = offsideLine })) nextPos
                | Failure (reason, span) -> Failure (reason, span)
        )

    let infixOp prec : PackratParser<Token, Token.Symbol> =
        AcceptMatch (fun t -> match t with :? Token.Symbol as sym when sym.precedence = prec -> Some(sym) | _ -> None)

    let tid: PackratParser<Token, Token.Id> = AcceptMatch (fun t -> match t with :? Token.Id as id -> Some(id) | _ -> None)

    let keyword kw: PackratParser<Token, Token.Keyword> = AcceptMatch (fun t -> match t with :? Token.Keyword as st when st.str = kw -> Some(st) | _ -> None)
    let delim d: PackratParser<Token, Token.Delim> = AcceptMatch (fun t -> match t with :? Token.Delim as st when st.char = d -> Some(st) | _ -> None)
    let symbol sym: PackratParser<Token, Token.Symbol> = AcceptMatch (fun t -> match t with :? Token.Symbol as st when st.str = sym -> Some(st) | _ -> None)
    
    // 基本 Expr パーサ（非再帰）
    let id = tid |>> fun id -> Ast.Expr.Id (id.str, id.span)
    let int: PackratParser<Token, Ast.Expr.Int> = AcceptMatch (fun t -> match t with :? Token.Int as st -> Some(Ast.Expr.Int(st.value, st.span)) | _ -> None)
    let float: PackratParser<Token, Ast.Expr.Float> = AcceptMatch (fun t -> match t with :? Token.Float as st -> Some(Ast.Expr.Float(st.value, st.span)) | _ -> None)
    let str: PackratParser<Token, Ast.Expr.String> = AcceptMatch (fun t -> match t with :? Token.String as st -> Some(Ast.Expr.String(st.value, st.span)) | _ -> None)
    let rec paren (): PackratParser<Token, Ast.Expr> =
        Delay (fun () -> 
            delim '(' &> expr () <& delim ')'
        )
    and doExpr (): PackratParser<Token, Ast.Expr> = 
        Delay (fun () -> 
            block (asToken (keyword "do")) (Once ((Many1 (stmt ()) |>> fun stmts -> Ast.Expr.Block(stmts, { left = stmts.Head.span.left; right = (List.last stmts).span.right })) <& Eoi) (fun (msg, span) -> Ast.Expr.Error(msg, span) :> Ast.Expr))
        )

    and factor (): PackratParser<Token, Ast.Expr> =
        Delay (fun () -> 
            doExpr () <|> (asExpr id) <|> (asExpr float) <|> (asExpr int) <|> (asExpr str)
        )

    and memberAccess (): PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            factor() <& symbol "." <&> tid |>> fun (expr, id) -> Ast.Expr.MemberAccess (expr, id.str, { left = expr.span.left; right = id.span.right })
        )
        
    and staticAccess (): PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            tid <& delim ':' <& delim ':' <&> tid |>> fun (typ, id) -> Ast.Expr.StaticAccess (typ.str, id.str, { left = typ.span.left; right = id.span.right })
        )
        
    // 呼び出し式の項
    and term1 (): PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            staticAccess () <|> memberAccess () <|> factor() 
        )

    // 呼び出し式
    and term2 (): PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            Many1 (term1 ()) |>> fun terms -> if terms.Length > 1
                                                  then Ast.Expr.Apply (terms.Head, terms.Tail, { left = terms.Head.span.left; right = (List.rev terms).Head.span.right })
                                                  else terms.Head
        )

    // 二項演算
    and binopExpr (): PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            List.fold (fun acc prec -> 
                let op = infixOp prec
                acc <&> (Optional (op <&> acc)) |>> fun (left, opt) -> 
                    match opt with
                    | Some (op, right) -> Ast.Expr.Apply (Ast.Expr.Id(op.str, op.span), [left; right], { left = left.span.left; right = right.span.right })
                    | None -> left
            ) (term2 ()) [9 .. -1 .. 0]
        )

    and expr (): PackratParser<Token, Ast.Expr> =
        Delay (fun () -> 
            binopExpr ()
        )

    // Stmt パーサ群（関数化）
    and letStmt (): PackratParser<Token, Ast.Stmt> =
        block (asToken (keyword "let")) (Once (tid <& symbol "=" <&> expr() |>> fun (id, rhs) -> Ast.Stmt.Let (id.str, rhs, { left = id.span.left; right = rhs.span.right})) (fun (msg, span) -> Ast.Stmt.Error(msg, span) :> Ast.Stmt))

    and varStmt (): PackratParser<Token, Ast.Stmt> =
        block (asToken (keyword "var")) (Once (tid <& symbol "=" <&> expr() |>> fun (id, rhs) -> Ast.Stmt.Var (id.str, rhs, { left = id.span.left; right = rhs.span.right})) (fun (msg, span) -> Ast.Stmt.Error(msg, span) :> Ast.Stmt))

    and assignStmt (): PackratParser<Token, Ast.Stmt> =
        tid <& symbol "=" <&> expr() |>> fun (id, rhs) -> Ast.Stmt.Assign (id.str, rhs, { left = id.span.left; right = rhs.span.right })

    and exprStmt (): PackratParser<Token, Ast.Stmt> =
        expr() |>> fun e -> Ast.Stmt.ExprStmt (e, e.span)

    and returnStmt (): PackratParser<Token, Ast.Stmt> =
        keyword "return" &> expr() |>> fun e -> Ast.Stmt.Return (e, e.span)

    and stmt (): PackratParser<Token, Ast.Stmt> =
        Delay (fun () -> 
            letStmt() <|> varStmt() <|> assignStmt() <|> exprStmt()
        )

    and typeExpr (): PackratParser<Token, Ast.TypeExpr> =
        Delay (fun () ->
            tid |>> fun id -> Ast.TypeExpr.Id (id.str, id.span)
        )

    and dataField (): PackratParser<Token, Ast.DataItem.Field> =
        Delay (fun () ->
            tid <& symbol ":" <&> typeExpr() |>> fun (id, typeExpr) -> Ast.DataItem.Field (id.str, typeExpr, { left = id.span.left; right = typeExpr.span.right })
        )

    and dataItem (): PackratParser<Token, Ast.DataItem> =
        Delay (fun () ->
            (asDataItem (dataField ()))
        )

    and dataDecl (): PackratParser<Token, Ast.Decl> =
        Delay (fun () ->
            block (asToken (keyword "data")) (Once (tid <& symbol "=" <&> Many1 (dataItem ()) |>> fun (id, items) -> Ast.Decl.Data (id.str, items, { left = id.span.left; right = (List.last items).span.right })) (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl))
        )

    and importDecl (): PackratParser<Token, Ast.Decl> =
        Delay (fun () ->
            block (asToken (keyword "import")) (Once (SepBy1 tid (symbol ".") |>> fun ids -> Ast.Decl.Import (ids |> List.map (fun id -> id.str), { left = ids.Head.span.left; right = (List.last ids).span.right })) (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl))
        )

    and fnArgNamed (): PackratParser<Token, Ast.FnArg> =
        Delay (fun () ->
            delim '(' &> tid <& symbol ":" <&> typeExpr() <& delim ')' |>> fun (id, typeExpr) -> Ast.FnArg.Named(id.str, typeExpr, { left = id.span.left; right = typeExpr.span.right })
        )

    and fnArgUnit (): PackratParser<Token, Ast.FnArg> =
        Delay (fun () ->
            delim '(' <&> delim ')' |>> fun (l,r) -> Ast.FnArg.Unit { left = l.span.left; right = r.span.right }
        )

    and fnArg (): PackratParser<Token, Ast.FnArg> =
        Delay (fun () ->
            fnArgUnit () <|> fnArgNamed ()
        )    

    and fnDecl (): PackratParser<Token, Ast.Decl> =
        Delay (fun () ->
            block (asToken (keyword "fn")) (Once (tid <&> Many (fnArg ()) <& delim ':' <&> typeExpr () <& symbol "=" <&> expr () |>> fun (((id, args), ret), body) -> Ast.Decl.Fn (id.str, args, ret, body, { left = id.span.left; right = body.span.right })) (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl))
        )

    and decl (): PackratParser<Token, Ast.Decl> =
        Delay (fun () ->
            dataDecl () <|> importDecl () <|> fnDecl ()
        )

    and fileModule (): PackratParser<Token, Ast.Module> =
        Delay (fun () ->
            Many (decl ()) |>> fun (decls) -> Ast.Module (decls)
        )

