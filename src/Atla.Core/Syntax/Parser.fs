namespace Atla.Core.Syntax

// FS0040: 相互再帰する値定義の初期化健全性チェックの警告を抑制する。
// Memo コンビネータがクロージャをすぐに評価しないため、実行時に問題は発生しない。
#nowarn "40"

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
    let asFnDecl (p: PackratParser<Token, Ast.Decl.Fn>) : PackratParser<Token, Ast.Decl> =
        p |>> fun f -> f :> Ast.Decl

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
    let thisKeywordId: PackratParser<Token, Token.Keyword> = AcceptMatch (fun t -> match t with :? Token.Keyword as kw when kw.str = "this" -> Some(kw) | _ -> None)
    let valueIdent: PackratParser<Token, string * Span> =
        (tid |>> fun id -> id.str, id.span)
        <|> (thisKeywordId |>> fun kw -> kw.str, kw.span)

    let keyword kw: PackratParser<Token, Token.Keyword> = AcceptMatch (fun t -> match t with :? Token.Keyword as st when st.str = kw -> Some(st) | _ -> None)
    let delim d: PackratParser<Token, Token.Delim> = AcceptMatch (fun t -> match t with :? Token.Delim as st when st.char = d -> Some(st) | _ -> None)
    let symbol sym: PackratParser<Token, Token.Symbol> = AcceptMatch (fun t -> match t with :? Token.Symbol as st when st.str = sym -> Some(st) | _ -> None)
    
    // 式
    let id =
        valueIdent
        |>> fun (name, span) -> Ast.Expr.Id (name, span)
    let int: PackratParser<Token, Ast.Expr.Int> = AcceptMatch (fun t -> match t with :? Token.Int as st -> Some(Ast.Expr.Int(st.value, st.span)) | _ -> None)
    let float: PackratParser<Token, Ast.Expr.Float> = AcceptMatch (fun t -> match t with :? Token.Float as st -> Some(Ast.Expr.Float(st.value, st.span)) | _ -> None)
    let str: PackratParser<Token, Ast.Expr.String> = AcceptMatch (fun t -> match t with :? Token.String as st -> Some(Ast.Expr.String(st.value, st.span)) | _ -> None)
    /// `true` / `false` キーワードを Bool リテラルとして解析する。
    let bool: PackratParser<Token, Ast.Expr.Bool> =
        AcceptMatch (fun t ->
            match t with
            | :? Token.Keyword as kw when kw.str = "True"  -> Some(Ast.Expr.Bool(true,  kw.span))
            | :? Token.Keyword as kw when kw.str = "False" -> Some(Ast.Expr.Bool(false, kw.span))
            | _ -> None)
    let unit: PackratParser<Token, Ast.Expr> = delim '(' <&> delim ')' |>> fun (l, r) -> Ast.Expr.Unit({ left = l.span.left; right = r.span.right })

    // 各パーサを安定した値（let rec ... and ...）として定義し、Memo で包む。
    // これにより各パーサルールは解析セッション全体を通じて単一のメモテーブルを持ち、
    // 同じ入力・同一位置への繰り返し呼び出しが即座にキャッシュから返される（パックラット解析）。
    // 相互再帰は let rec ... and ... で処理される。クロージャ内では
    // 他のパーサを即座に評価せず、解析呼び出し時まで遅延させるため、
    // 全バインディング初期化後の値として正しく参照される。
    let rec paren: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            (delim '(' &> expr <& delim ')') input pos
        )
    and doExpr: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            block (asToken (keyword "do")) (Once ((Many1 stmt |>> fun stmts -> Ast.Expr.Block(stmts, { left = stmts.Head.span.left; right = (List.last stmts).span.right })) <& Eoi) (fun (msg, span) -> Ast.Expr.Error(msg, span) :> Ast.Expr)) input pos
        )

    // if式
    and ifThen: PackratParser<Token, Ast.IfBranch> =
        Memo (fun input pos ->
            block (asToken (symbol "|")) (expr <& keyword "=>" <&> (Once expr (fun (msg, span) -> Ast.Expr.Error(msg, span))) |>> fun (cond, body) -> Ast.IfBranch.Then(cond, body, { left = cond.span.left; right = body.span.right })) input pos
        )

    and ifElse: PackratParser<Token, Ast.IfBranch> =
        Memo (fun input pos ->
            block (asToken (symbol "|")) (keyword "else" &> keyword "=>" &> (Once expr (fun (msg, span) -> Ast.Expr.Error(msg, span))) |>> fun (body) -> Ast.IfBranch.Else(body, body.span)) input pos
        )

    and ifExpr: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            block (asToken (keyword "if")) (Once (Many1 ifThen <&> ifElse |>> fun (branches, elseBranch) -> Ast.Expr.If(branches @ [elseBranch], { left = branches.Head.span.left; right = (List.last branches).span.right })) (fun (msg, span) -> Ast.Expr.Error(msg, span) :> Ast.Expr)) input pos
        )

    // 項
    and dataInitField: PackratParser<Token, Ast.DataInitField> =
        Memo (fun input pos ->
            (tid <& symbol "=" <&> expr
            |>> fun (fieldId, fieldValue) ->
                Ast.DataInitField.Field(fieldId.str, fieldValue, { left = fieldId.span.left; right = fieldValue.span.right }) :> Ast.DataInitField) input pos
        )

    // `TypeName { field = value, ... }` 形式の data 初期化式を解析する。
    and dataInitExpr: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            (tid <& delim '{' <&> SepBy1 dataInitField (delim ',') <&> delim '}'
            |>> fun ((typeId, fields), closeBrace) ->
                Ast.Expr.DataInit(typeId.str, fields, { left = typeId.span.left; right = closeBrace.span.right }) :> Ast.Expr) input pos
        )

    and factor: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            (unit <|> paren <|> ifExpr <|> doExpr <|> dataInitExpr <|> (asExpr id) <|> (asExpr float) <|> (asExpr int) <|> (asExpr str) <|> (asExpr bool)) input pos
        )

    and postfixMemberAccess: PackratParser<Token, (Ast.Expr -> Ast.Expr)> =
        Memo (fun input pos ->
            match (delim '\'') input pos with
            | Failure (reason, span) -> Failure (reason, span)
            | Success (quote, afterQuotePos) ->
                match tid input afterQuotePos with
                | Success (id, nextPos) ->
                    Success
                        ((fun receiver -> Ast.Expr.MemberAccess(receiver, id.str, { left = receiver.span.left; right = id.span.right }) :> Ast.Expr),
                         nextPos)
                | Failure _ ->
                    // `a'` のようにメンバ名が欠けている場合は、式エラーへ正規化する。
                    Success
                        ((fun receiver ->
                            Ast.Expr.Error("Expected member identifier after apostrophe.", { left = receiver.span.left; right = quote.span.right }) :> Ast.Expr),
                         afterQuotePos)
        )

    // 型引数付き呼び出しの postfix を受理する（例: receiver[Application]）。
    and postfixGenericApply: PackratParser<Token, (Ast.Expr -> Ast.Expr)> =
        Memo (fun input pos ->
            (delim '[' &> SepBy1 typeExpr (delim ',') <&> delim ']' |>> fun (typeArgs, closeBracket) ->
                fun receiver ->
                    Ast.Expr.GenericApply(receiver, typeArgs, { left = receiver.span.left; right = closeBracket.span.right }) :> Ast.Expr) input pos
        )

    and postfixExpr: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            (factor <&> Many (postfixMemberAccess <|> postfixGenericApply)
            |>> fun (headExpr, postfixes) -> List.fold (fun current applyPostfix -> applyPostfix current) headExpr postfixes) input pos
        )

    // 代入左辺として許可される式を解析する。
    // 許可: 識別子、アポストロフィによるメンバーアクセス連鎖（例: a'b'c）。
    // 非許可: 呼び出し結果、リテラル、型引数適用、添字アクセス。
    and assignLValueExpr: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            ((asExpr id) <&> Many postfixMemberAccess
            |>> fun (headExpr, postfixes) -> List.fold (fun current applyPostfix -> applyPostfix current) headExpr postfixes) input pos
        )

    // 単項マイナスを解析し、AST の既存 Apply 形状へ正規化する。
    // - 負の数値リテラルは literal 値へ直接畳み込む。
    // - それ以外の `-expr` は `0 - expr` 呼び出し形へ変換する。
    and unaryTerm: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            match (symbol "-") input pos with
            | Success (minusToken, afterMinusPos) ->
                match unaryTerm input afterMinusPos with
                | Success (operandExpr, nextPos) ->
                    match operandExpr with
                    | :? Ast.Expr.Int as intExpr ->
                        Success (Ast.Expr.Int(-intExpr.value, { left = minusToken.span.left; right = intExpr.span.right }) :> Ast.Expr, nextPos)
                    | :? Ast.Expr.Float as floatExpr ->
                        Success (Ast.Expr.Float(-floatExpr.value, { left = minusToken.span.left; right = floatExpr.span.right }) :> Ast.Expr, nextPos)
                    | _ ->
                        let zeroExpr = Ast.Expr.Int(0, minusToken.span) :> Ast.Expr
                        let negatedExpr =
                            Ast.Expr.Apply(
                                Ast.Expr.Id("-", minusToken.span) :> Ast.Expr,
                                [ zeroExpr; operandExpr ],
                                { left = minusToken.span.left; right = operandExpr.span.right }) :> Ast.Expr
                        Success (negatedExpr, nextPos)
                | Failure (reason, span) -> Failure (reason, span)
            | Failure _ ->
                postfixExpr input pos
        )

    // 呼び出し式の項
    and term1: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            // TODO: 将来的には `!!` を通常の演算子解決（Id("!!") 経由）へ統一し、
            //       term1 での IndexAccess 特別扱いを削除する。
            (unaryTerm <&> Optional (symbol "!!" &> unaryTerm)
            |>> fun (receiver, optIndex) ->
                match optIndex with
                | Some index ->
                    Ast.Expr.IndexAccess(receiver, index, { left = receiver.span.left; right = index.span.right }) :> Ast.Expr
                | None ->
                    receiver) input pos
        )

    // Dot-only 呼び出し式を解析する。
    // 仕様:
    // - `f.` は 0 引数呼び出し。
    // - `x .` は callable 式 `x` の 0 引数呼び出し。
    // - `x f.` は `f(x)` に正規化される。
    // - `a b f.` は `f(a, b)` に正規化される。
    // - `x f. g.` は `g(f(x))` に正規化される。
    // - `x f` / `a b f` は parse error（callee の直後に `.` 必須）。
    and term2: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            match term1 input pos with
            | Failure (reason, span) -> Failure (reason, span)
            | Success (head, afterHeadPos) ->
                // 同一インデントの次文を誤って取り込まないよう、head 開始位置を基準に可視範囲を制限する。
                let callInput = BlockInput(input, head.span.left) :> Input<Token>
                // 1 ステップ分の dot 呼び出しチェーンを適用する。
                let rec loop (current: Ast.Expr) (currentPos: Position) =
                    match (symbol ".") callInput currentPos with
                    | Success (dot, afterDotPos) ->
                        // `current.` / `current .` を 0 引数呼び出しとして扱う。
                        let zeroArgCall =
                            Ast.Expr.Apply(current, [], { left = current.span.left; right = dot.span.right }) :> Ast.Expr
                        loop zeroArgCall afterDotPos
                    | Failure _ ->
                        // `current` の後続を `arg* callee .` 形式として読み取る。
                        // 先頭から最後の項までを収集し、最後の項を callee、手前を追加引数として扱う。
                        let rec collectTailTerms (tailPos: Position) (acc: Ast.Expr list) =
                            // 二項演算子位置では dot-call 引数収集を打ち切り、binop 解析へ制御を戻す。
                            // これにより `n - 2` の `-` を誤って unary 引数として取り込むことを防ぐ。
                            match callInput.get tailPos with
                            | Some (:? Token.Symbol as sym) when sym.str <> "!!" && sym.str <> "." ->
                                List.rev acc, tailPos
                            | _ ->
                                match term1 callInput tailPos with
                                | Success (parsed, nextPos) -> collectTailTerms nextPos (parsed :: acc)
                                | Failure _ -> List.rev acc, tailPos

                        let tailTerms, afterTailTermsPos = collectTailTerms currentPos []
                        match tailTerms with
                        | [] ->
                            Success (current, currentPos)
                        | _ ->
                            match (symbol ".") callInput afterTailTermsPos with
                            | Success (dot, afterDotPos) ->
                                // `current arg1 ... callee.` を `callee(current, arg1, ...)` へ正規化する。
                                let callee = List.last tailTerms
                                let extraArgs = tailTerms |> List.take (tailTerms.Length - 1)
                                let allArgs = current :: extraArgs
                                let pipedCall =
                                    Ast.Expr.Apply(callee, allArgs, { left = current.span.left; right = dot.span.right }) :> Ast.Expr
                                loop pipedCall afterDotPos
                            | Failure _ ->
                                // `current arg* callee` は `.` 必須ルール違反として式エラーを返す。
                                let callee = List.last tailTerms
                                Success
                                    (Ast.Expr.Error("Expected '.' after callee in call expression.", { left = current.span.left; right = callee.span.right }) :> Ast.Expr,
                                     afterTailTermsPos)
                loop head afterHeadPos
        )

    // 二項演算
    and binopExpr: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            (List.fold (fun acc prec -> 
                let op = infixOp prec
                acc <&> (Optional (op <&> acc)) |>> fun (left, opt) -> 
                    match opt with
                    | Some (op, right) -> Ast.Expr.Apply (Ast.Expr.Id(op.str, op.span), [left; right], { left = left.span.left; right = right.span.right }) :> Ast.Expr
                    | None -> left
            ) term2 [9 .. -1 .. 0]) input pos
        )

    // lambda 引数名の重複を検証し、最初に見つかった重複名を返す。
    and private tryFindDuplicateArgName (argNames: string list) : string option =
        let folder (seen, duplicated) argName =
            match duplicated with
            | Some _ -> seen, duplicated
            | None when Set.contains argName seen -> seen, Some argName
            | None -> Set.add argName seen, None
        let _, duplicated = List.fold folder (Set.empty, None) argNames
        duplicated

    // 無名関数式（例: fn x y -> expr, fn () -> expr）を解析する。
    // 仕様:
    // - 引数は Id の並び、または明示ユニット引数 `()` のみを許可する。
    // - `fn -> expr` は空引数エラーとして Ast.Expr.Error を返す。
    // - 引数重複は Ast.Expr.Error を返す。
    and lambdaExpr: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            match (keyword "fn") input pos with
            | Failure (reason, span) -> Failure (reason, span)
            | Success (fnKw, afterFnPos) ->
                let lambdaInput = BlockInput(input, fnKw.span.left) :> Input<Token>
                let unitArgParse = (delim '(' <&> delim ')') lambdaInput afterFnPos

                // 引数列を（Id* もしくは `()`）として読み取る。
                let (argNames, afterArgsPos, usedUnitSyntax) =
                    match unitArgParse with
                    | Success (_, afterUnitPos) -> [], afterUnitPos, true
                    | Failure _ ->
                        let rec collectArgNames (currentPos: Position) (acc: string list) =
                            match tid lambdaInput currentPos with
                            | Success (id, nextPos) -> collectArgNames nextPos (id.str :: acc)
                            | Failure _ -> List.rev acc, currentPos
                        let names, endPos = collectArgNames afterFnPos []
                        names, endPos, false

                match (keyword "->") lambdaInput afterArgsPos with
                | Failure (reason, span) -> Failure (reason, span)
                | Success (_, afterArrowPos) ->
                    match expr lambdaInput afterArrowPos with
                    | Failure (msg, span) ->
                        Success (Ast.Expr.Error(msg, span) :> Ast.Expr, afterArrowPos)
                    | Success (bodyExpr, nextPos) ->
                        let lambdaSpan = { left = fnKw.span.left; right = bodyExpr.span.right }
                        match usedUnitSyntax, argNames with
                        | false, [] ->
                            Success (Ast.Expr.Error("Lambda parameter list is empty. Use at least one identifier or explicit unit argument list '()'.", lambdaSpan) :> Ast.Expr, nextPos)
                        | _ ->
                            match tryFindDuplicateArgName argNames with
                            | Some duplicatedName ->
                                Success (Ast.Expr.Error(sprintf "Duplicate lambda parameter '%s'" duplicatedName, lambdaSpan) :> Ast.Expr, nextPos)
                            | None ->
                                Success (Ast.Expr.Lambda(argNames, bodyExpr, lambdaSpan) :> Ast.Expr, nextPos)
        )

    and expr: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            (lambdaExpr <|> binopExpr) input pos
        )

    // 文
    and letStmt: PackratParser<Token, Ast.Stmt> =
        Memo (fun input pos ->
            block (asToken (keyword "let")) (Once (tid <& symbol "=" <&> expr |>> fun (id, rhs) -> Ast.Stmt.Let (id.str, rhs, { left = id.span.left; right = rhs.span.right})) (fun (msg, span) -> Ast.Stmt.Error(msg, span) :> Ast.Stmt)) input pos
        )

    and varStmt: PackratParser<Token, Ast.Stmt> =
        Memo (fun input pos ->
            block (asToken (keyword "var")) (Once (tid <& symbol "=" <&> expr |>> fun (id, rhs) -> Ast.Stmt.Var (id.str, rhs, { left = id.span.left; right = rhs.span.right})) (fun (msg, span) -> Ast.Stmt.Error(msg, span) :> Ast.Stmt)) input pos
        )

    and assignStmt: PackratParser<Token, Ast.Stmt> =
        Memo (fun input pos ->
            (assignLValueExpr <& symbol "=" <&> expr
            |>> fun (target, rhs) -> Ast.Stmt.Assign (target, rhs, { left = target.span.left; right = rhs.span.right }) :> Ast.Stmt) input pos
        )

    and exprStmt: PackratParser<Token, Ast.Stmt> =
        Memo (fun input pos ->
            (expr |>> fun e -> Ast.Stmt.ExprStmt (e, e.span) :> Ast.Stmt) input pos
        )

    and returnStmt: PackratParser<Token, Ast.Stmt> =
        Memo (fun input pos ->
            (keyword "return" &> expr |>> fun e -> Ast.Stmt.Return (e, e.span) :> Ast.Stmt) input pos
        )

    and forStmt: PackratParser<Token, Ast.Stmt> =
        Memo (fun input pos ->
            match (keyword "for") input pos with
            | Success (forKw, afterForPos) ->
                let bodyInput = BlockInput(input, forKw.span.left) :> Input<Token>

                match tid bodyInput afterForPos with
                | Success (id, afterIdPos) ->
                    match (keyword "in") bodyInput afterIdPos with
                    | Success (_, afterInPos) ->
                        let iterableInput = LineInput(bodyInput, forKw.span.left.Line) :> Input<Token>

                        match expr iterableInput afterInPos with
                        | Success (iterable, afterIterablePos) ->
                            let bodyStartPos =
                                match (keyword "=>") bodyInput afterIterablePos with
                                | Success (_, afterArrowPos) -> afterArrowPos
                                | Failure _ -> afterIterablePos

                            match Many1 stmt bodyInput bodyStartPos with
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

    and stmt: PackratParser<Token, Ast.Stmt> =
        Memo (fun input pos ->
            (letStmt <|> varStmt <|> returnStmt <|> forStmt <|> assignStmt <|> exprStmt) input pos
        )

    and typeExprUnit: PackratParser<Token, Ast.TypeExpr> =
        Memo (fun input pos ->
            (delim '(' <&> delim ')' |>> fun (l,r) -> Ast.TypeExpr.Unit ({ left = l.span.left; right = r.span.right }) :> Ast.TypeExpr) input pos
        )

    and typeExprId: PackratParser<Token, Ast.TypeExpr> =
        Memo (fun input pos ->
            (tid |>> fun id -> Ast.TypeExpr.Id (id.str, id.span) :> Ast.TypeExpr) input pos
        )
        
    and typeExprAtom: PackratParser<Token, Ast.TypeExpr> =
        Memo (fun input pos ->
            ((asTypeExpr typeExprUnit) <|> (asTypeExpr typeExprId)) input pos
        )

    // 空白区切りの型適用（例: Array String）を左結合で畳み込む。
    and typeExprApply: PackratParser<Token, Ast.TypeExpr> =
        Memo (fun input pos ->
            (Many1 typeExprAtom
            |>> fun typeExprs ->
                match typeExprs with
                | [] ->
                    // Many1 により通常は到達しないが、網羅性警告を避けるため防御的に失敗させる。
                    failwith "type expression list must not be empty"
                | head :: [] -> head
                | head :: tail ->
                    Ast.TypeExpr.Apply(head, tail, { left = head.span.left; right = (List.last tail).span.right }) :> Ast.TypeExpr) input pos
        )

    // 関数型（例: Int -> Int）を右結合でパースする。
    // typeExpr := typeExprApply ('->' typeExpr)?
    and typeExpr: PackratParser<Token, Ast.TypeExpr> =
        Memo (fun input pos ->
            (typeExprApply <&> Optional (keyword "->" &> typeExpr)
            |>> fun (argType, retOpt) ->
                match retOpt with
                | None -> argType
                | Some retType ->
                    Ast.TypeExpr.Arrow(argType, retType, { left = argType.span.left; right = retType.span.right }) :> Ast.TypeExpr) input pos
        )

    // データ宣言
    and dataField: PackratParser<Token, Ast.DataItem.Field> =
        Memo (fun input pos ->
            (tid <& delim ':' <&> typeExpr |>> fun (id, typeExpr) -> Ast.DataItem.Field (id.str, typeExpr, { left = id.span.left; right = typeExpr.span.right })) input pos
        )

    and dataItem: PackratParser<Token, Ast.DataItem> =
        Memo (fun input pos ->
            (asDataItem dataField) input pos
        )

    and dataDecl: PackratParser<Token, Ast.Decl> =
        Memo (fun input pos ->
            (keyword "data"
            &> Once (
                tid <& symbol "=" <& delim '{' <&> SepBy1 dataItem (delim ',') <&> delim '}'
                |>> fun ((id, items), closeBrace) ->
                    Ast.Decl.Data (id.str, items, { left = id.span.left; right = closeBrace.span.right }) :> Ast.Decl
            ) (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl)) input pos
        )

    // インポート宣言
    and importDecl: PackratParser<Token, Ast.Decl> =
        Memo (fun input pos ->
            block (asToken (keyword "import")) (Once (SepBy1 tid (delim '\'') |>> fun ids -> Ast.Decl.Import (ids |> List.map (fun id -> id.str), { left = ids.Head.span.left; right = (List.last ids).span.right })) (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl)) input pos
        )

    // 関数宣言
    and fnArgNamed: PackratParser<Token, Ast.FnArg> =
        Memo (fun input pos ->
            (delim '(' &> valueIdent <& delim ':' <&> typeExpr <& delim ')' |>> fun ((name, nameSpan), typeExpr) -> Ast.FnArg.Named(name, typeExpr, { left = nameSpan.left; right = typeExpr.span.right }) :> Ast.FnArg) input pos
        )

    and fnArgUnit: PackratParser<Token, Ast.FnArg> =
        Memo (fun input pos ->
            (delim '(' <&> delim ')' |>> fun (l,r) -> Ast.FnArg.Unit { left = l.span.left; right = r.span.right } :> Ast.FnArg) input pos
        )

    and fnArg: PackratParser<Token, Ast.FnArg> =
        Memo (fun input pos ->
            (fnArgUnit <|> fnArgNamed) input pos
        )

    and fnBodyExpr: PackratParser<Token, Ast.Expr> =
        Memo (fun input pos ->
            (expr
            <|> (stmt |>> fun singleStmt -> Ast.Expr.Block([singleStmt], { left = singleStmt.span.left; right = singleStmt.span.right }) :> Ast.Expr)) input pos
        )

    and fnDecl: PackratParser<Token, Ast.Decl> =
        Memo (fun input pos ->
            block (asToken (keyword "fn")) (Once (tid <&> Many fnArg <& delim ':' <&> typeExpr <& symbol "=" <&> fnBodyExpr |>> fun (((id, args), ret), body) -> Ast.Decl.Fn (id.str, args, ret, body, { left = id.span.left; right = body.span.right })) (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl)) input pos
        )

    and implMethodDecl: PackratParser<Token, Ast.Decl.Fn> =
        Memo (fun input pos ->
            block (asToken (keyword "fn"))
                    (Once
                    (tid <&> Many fnArg <& delim ':' <&> typeExpr <& symbol "=" <&> fnBodyExpr
                     |>> fun (((id, args), ret), body) -> Ast.Decl.Fn(id.str, args, ret, body, { left = id.span.left; right = body.span.right }))
                    (fun (msg, span) ->
                        Ast.Decl.Fn($"error_{span.left.Line}_{span.left.Column}", [], Ast.TypeExpr.Id("Unit", span), Ast.Expr.Error(msg, span), span))) input pos
        )

    and implDecl: PackratParser<Token, Ast.Decl> =
        // `impl A as B` と `impl A [for Role] [by field]` は相互に排他的な構文。
        // `as` を検出した場合は .NET 継承形式として処理し、`for`/`by` は禁止する。
        Memo (fun input pos ->
            block (asToken (keyword "impl"))
                (Once
                    ((   // 分岐1: impl A as B（for/by なし）
                         tid <& keyword "as" <&> tid <&> Many (asFnDecl implMethodDecl) |>> fun ((typeId, asTypeId), methodDecls) ->
                             let methods =
                                 methodDecls
                                 |> List.choose (fun methodDecl ->
                                     match methodDecl with
                                     | :? Ast.Decl.Fn as fn -> Some fn
                                     | _ -> None)
                             let rightSpan =
                                 match methods |> List.tryLast with
                                 | Some lastMethod -> lastMethod.span.right
                                 | None -> asTypeId.span.right
                             Ast.Decl.Impl(typeId.str, Some asTypeId.str, None, None, methods, { left = typeId.span.left; right = rightSpan }) :> Ast.Decl)
                     <|> // 分岐2: 既存構文 impl A [for Role] [by field]
                         (tid <&> Optional (keyword "for" &> tid) <&> Optional (keyword "by" &> tid) <&> Many (asFnDecl implMethodDecl) |>> fun (((typeId, forTypeIdOpt), byFieldIdOpt), methodDecls) ->
                             let methods =
                                 methodDecls
                                 |> List.choose (fun methodDecl ->
                                     match methodDecl with
                                     | :? Ast.Decl.Fn as fn -> Some fn
                                     | _ -> None)
                             let rightSpan =
                                 match methods |> List.tryLast with
                                 | Some lastMethod -> lastMethod.span.right
                                 | None -> typeId.span.right
                             let forTypeName = forTypeIdOpt |> Option.map (fun forTypeId -> forTypeId.str)
                             let byFieldName = byFieldIdOpt |> Option.map (fun byFieldId -> byFieldId.str)
                             Ast.Decl.Impl(typeId.str, None, forTypeName, byFieldName, methods, { left = typeId.span.left; right = rightSpan }) :> Ast.Decl))
                    (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl)) input pos
        )

    and decl: PackratParser<Token, Ast.Decl> =
        Memo (fun input pos ->
            (dataDecl <|> importDecl <|> fnDecl <|> implDecl) input pos
        )

    // モジュール
    and fileModule: PackratParser<Token, Ast.Module> =
        Memo (fun input pos ->
            (Many decl <& Eoi |>> fun decls -> Ast.Module (decls)) input pos
        )
