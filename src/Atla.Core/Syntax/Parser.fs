namespace Atla.Core.Syntax

open Atla.Core.Data
open Atla.Core.Syntax.Data
open Atla.Core.Syntax.Combinators

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

type LineInput (tokenInput: Input<Token>, line: int) =
    interface Input<Token> with
        member _.get (arg: Position): Token option =
            if arg.Line > line then None
            else tokenInput.get arg

        member _.next (arg: Position): Position =
            if arg.Line > line then
                { Line = arg.Line; Column = arg.Column + 1 }
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
    let asTypeExpr<'T when 'T :> Ast.TypeExpr> (p: PackratParser<Token, 'T>) : PackratParser<Token, Ast.TypeExpr> =
        p |>> fun t -> t :> Ast.TypeExpr

    let block<'A> (opener: PackratParser<Token, Token>) (body: PackratParser<Token, 'A>): PackratParser<Token, 'A> =
        Delay (fun () -> fun input pos ->
            match opener input pos with
                | Success (token, nextPos) -> 
                    // 先頭のトークンを探す
                    let offsideLine = [| 0 .. token.span.left.Column |] |> Array.find (fun i -> (input.get { Line = token.span.left.Line; Column = i }).IsSome)
                    body (BlockInput (input, { Line = token.span.left.Line; Column = offsideLine })) nextPos
                | Failure (reason, span) -> Failure (reason, span)
        )

    // 通常の二項演算子を受理する（index専用演算子 "!!" は除外する）。
    let infixOp prec : PackratParser<Token, Token.Symbol> =
        AcceptMatch (fun t ->
            match t with
            | :? Token.Symbol as sym when sym.precedence = prec && sym.str <> "!!" -> Some(sym)
            | _ -> None)

    let tid: PackratParser<Token, Token.Id> = AcceptMatch (fun t -> match t with :? Token.Id as id -> Some(id) | _ -> None)

    let keyword kw: PackratParser<Token, Token.Keyword> = AcceptMatch (fun t -> match t with :? Token.Keyword as st when st.str = kw -> Some(st) | _ -> None)
    let delim d: PackratParser<Token, Token.Delim> = AcceptMatch (fun t -> match t with :? Token.Delim as st when st.char = d -> Some(st) | _ -> None)
    let symbol sym: PackratParser<Token, Token.Symbol> = AcceptMatch (fun t -> match t with :? Token.Symbol as st when st.str = sym -> Some(st) | _ -> None)
    
    // 式
    let id = tid |>> fun id -> Ast.Expr.Id (id.str, id.span)
    let int: PackratParser<Token, Ast.Expr.Int> = AcceptMatch (fun t -> match t with :? Token.Int as st -> Some(Ast.Expr.Int(st.value, st.span)) | _ -> None)
    let float: PackratParser<Token, Ast.Expr.Float> = AcceptMatch (fun t -> match t with :? Token.Float as st -> Some(Ast.Expr.Float(st.value, st.span)) | _ -> None)
    let str: PackratParser<Token, Ast.Expr.String> = AcceptMatch (fun t -> match t with :? Token.String as st -> Some(Ast.Expr.String(st.value, st.span)) | _ -> None)
    let unit: PackratParser<Token, Ast.Expr> = delim '(' <&> delim ')' |>> fun (l, r) -> Ast.Expr.Unit({ left = l.span.left; right = r.span.right })
    let rec paren (): PackratParser<Token, Ast.Expr> =
        Delay (fun () -> 
            delim '(' &> expr () <& delim ')'
        )
    and doExpr (): PackratParser<Token, Ast.Expr> = 
        Delay (fun () -> 
            block (asToken (keyword "do")) (Once ((Many1 (stmt ()) |>> fun stmts -> Ast.Expr.Block(stmts, { left = stmts.Head.span.left; right = (List.last stmts).span.right })) <& Eoi) (fun (msg, span) -> Ast.Expr.Error(msg, span) :> Ast.Expr))
        )

    // if式
    and ifThen (): PackratParser<Token, Ast.IfBranch> =
        Delay (fun () ->
            block (asToken (symbol "|")) (expr() <& keyword "=>" <&> (Once (expr()) (fun (msg, span) -> Ast.Expr.Error(msg, span))) |>> fun (cond, body) -> Ast.IfBranch.Then(cond, body, { left = cond.span.left; right = body.span.right }))
        )

    and ifElse (): PackratParser<Token, Ast.IfBranch> =
        Delay (fun () ->
            block (asToken (symbol "|")) (keyword "else" &> keyword "=>" &> (Once (expr()) (fun (msg, span) -> Ast.Expr.Error(msg, span))) |>> fun (body) -> Ast.IfBranch.Else(body, body.span))
        )

    and ifExpr (): PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            block (asToken (keyword "if")) (Once (Many1 (ifThen ()) <&> (ifElse ()) |>> fun (branches, elseBranch) -> Ast.Expr.If(branches @ [elseBranch], { left = branches.Head.span.left; right = (List.last branches).span.right })) (fun (msg, span) -> Ast.Expr.Error(msg, span) :> Ast.Expr))
        )

    // 項
    and factor (): PackratParser<Token, Ast.Expr> =
        Delay (fun () -> 
            unit <|> paren () <|> ifExpr () <|> doExpr () <|> (asExpr id) <|> (asExpr float) <|> (asExpr int) <|> (asExpr str)
        )

    and staticAccess (): PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            tid <& delim ':' <& delim ':' <&> tid |>> fun (typ, id) -> Ast.Expr.StaticAccess (typ.str, id.str, { left = typ.span.left; right = id.span.right })
        )

    and postfixMemberAccess (): PackratParser<Token, (Ast.Expr -> Ast.Expr)> =
        Delay (fun () ->
            symbol "." &> tid |>> fun id ->
                fun receiver -> Ast.Expr.MemberAccess(receiver, id.str, { left = receiver.span.left; right = id.span.right }) :> Ast.Expr
        )

    // 型引数付き呼び出しの postfix を受理する（例: receiver[Application]）。
    and postfixGenericApply (): PackratParser<Token, (Ast.Expr -> Ast.Expr)> =
        Delay (fun () ->
            delim '[' &> SepBy1 (typeExpr ()) (delim ',') <&> delim ']' |>> fun (typeArgs, closeBracket) ->
                fun receiver ->
                    Ast.Expr.GenericApply(receiver, typeArgs, { left = receiver.span.left; right = closeBracket.span.right }) :> Ast.Expr
        )

    and postfixExpr (): PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            (staticAccess () <|> factor ()) <&> Many (postfixMemberAccess () <|> postfixGenericApply ())
            |>> fun (headExpr, postfixes) -> List.fold (fun current applyPostfix -> applyPostfix current) headExpr postfixes
        )

    // 呼び出し式の項
    and term1 (): PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            // TODO: 将来的には `!!` を通常の演算子解決（Id("!!") 経由）へ統一し、
            //       term1 での IndexAccess 特別扱いを削除する。
            postfixExpr () <&> Optional (symbol "!!" &> postfixExpr ())
            |>> fun (receiver, optIndex) ->
                match optIndex with
                | Some index ->
                    Ast.Expr.IndexAccess(receiver, index, { left = receiver.span.left; right = index.span.right }) :> Ast.Expr
                | None ->
                    receiver
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

    // 文
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

    and forStmt (): PackratParser<Token, Ast.Stmt> =
        Delay (fun () -> fun input pos ->
            match (keyword "for") input pos with
            | Success (forKw, afterForPos) ->
                let bodyInput = BlockInput(input, forKw.span.left) :> Input<Token>

                match tid bodyInput afterForPos with
                | Success (id, afterIdPos) ->
                    match (keyword "in") bodyInput afterIdPos with
                    | Success (_, afterInPos) ->
                        let iterableInput = LineInput(bodyInput, forKw.span.left.Line) :> Input<Token>

                        match expr () iterableInput afterInPos with
                        | Success (iterable, afterIterablePos) ->
                            let bodyStartPos =
                                match (keyword "=>") bodyInput afterIterablePos with
                                | Success (_, afterArrowPos) -> afterArrowPos
                                | Failure _ -> afterIterablePos

                            match Many1 (stmt ()) bodyInput bodyStartPos with
                            | Success (bodyStmts, nextPos) ->
                                Success (Ast.Stmt.For(id.str, iterable, bodyStmts, { left = id.span.left; right = (List.last bodyStmts).span.right }) :> Ast.Stmt, nextPos)
                            | Failure (msg, span) ->
                                Success (Ast.Stmt.Error(msg, span) :> Ast.Stmt, bodyStartPos)
                        | Failure (msg, span) ->
                            Success (Ast.Stmt.Error(msg, span) :> Ast.Stmt, afterInPos)
                    | Failure (msg, span) ->
                        Success (Ast.Stmt.Error(msg, span) :> Ast.Stmt, afterIdPos)
                | Failure (msg, span) ->
                    Success (Ast.Stmt.Error(msg, span) :> Ast.Stmt, afterForPos)
            | Failure (reason, span) -> Failure (reason, span)
        )

    and stmt (): PackratParser<Token, Ast.Stmt> =
        Delay (fun () -> 
            letStmt() <|> varStmt() <|> returnStmt() <|> forStmt() <|> assignStmt() <|> exprStmt()
        )

    and typeExprUnit (): PackratParser<Token, Ast.TypeExpr> =
        Delay (fun () ->
            delim '(' <&> delim ')' |>> fun (l,r) -> Ast.TypeExpr.Unit ({ left = l.span.left; right = r.span.right })
        )

    and typeExprId (): PackratParser<Token, Ast.TypeExpr> =
        Delay (fun () ->
            tid |>> fun id -> Ast.TypeExpr.Id (id.str, id.span)
        )
        
    and typeExprAtom (): PackratParser<Token, Ast.TypeExpr> =
        Delay (fun () ->
            (asTypeExpr (typeExprUnit ())) <|> (asTypeExpr (typeExprId ()))
        )

    and typeExpr (): PackratParser<Token, Ast.TypeExpr> =
        Delay (fun () ->
            // 空白区切りの型適用（例: Array String）を左結合で畳み込む。
            Many1 (typeExprAtom ())
            |>> fun typeExprs ->
                match typeExprs with
                | head :: [] -> head
                | head :: tail ->
                    Ast.TypeExpr.Apply(head, tail, { left = head.span.left; right = (List.last tail).span.right }) :> Ast.TypeExpr
        )

    // データ宣言
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

    // インポート宣言
    and importDecl (): PackratParser<Token, Ast.Decl> =
        Delay (fun () ->
            block (asToken (keyword "import")) (Once (SepBy1 tid (symbol ".") |>> fun ids -> Ast.Decl.Import (ids |> List.map (fun id -> id.str), { left = ids.Head.span.left; right = (List.last ids).span.right })) (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl))
        )

    // 関数宣言
    and fnArgNamed (): PackratParser<Token, Ast.FnArg> =
        Delay (fun () ->
            delim '(' &> tid <& delim ':' <&> typeExpr() <& delim ')' |>> fun (id, typeExpr) -> Ast.FnArg.Named(id.str, typeExpr, { left = id.span.left; right = typeExpr.span.right })
        )

    and fnArgUnit (): PackratParser<Token, Ast.FnArg> =
        Delay (fun () ->
            delim '(' <&> delim ')' |>> fun (l,r) -> Ast.FnArg.Unit { left = l.span.left; right = r.span.right }
        )

    and fnArg (): PackratParser<Token, Ast.FnArg> =
        Delay (fun () ->
            fnArgUnit () <|> fnArgNamed ()
        )

    and fnBodyExpr (): PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            (expr ())
            <|> (stmt () |>> fun singleStmt -> Ast.Expr.Block([singleStmt], { left = singleStmt.span.left; right = singleStmt.span.right }) :> Ast.Expr)
        )

    and fnDecl (): PackratParser<Token, Ast.Decl> =
        Delay (fun () ->
            block (asToken (keyword "fn")) (Once (tid <&> Many (fnArg ()) <& delim ':' <&> typeExpr () <& symbol "=" <&> fnBodyExpr () |>> fun (((id, args), ret), body) -> Ast.Decl.Fn (id.str, args, ret, body, { left = id.span.left; right = body.span.right })) (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl))
        )

    and decl (): PackratParser<Token, Ast.Decl> =
        Delay (fun () ->
            dataDecl () <|> importDecl () <|> fnDecl ()
        )

    // モジュール
    and fileModule (): PackratParser<Token, Ast.Module> =
        Delay (fun () ->
            Many (decl ()) |>> fun (decls) -> Ast.Module (decls)
        )
