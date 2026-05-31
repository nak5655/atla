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
    let asPattern<'P when 'P :> Ast.Pattern> (p: PackratParser<Token, 'P>) : PackratParser<Token, Ast.Pattern> =
        p |>> fun pattern -> pattern :> Ast.Pattern
    let asMatchArm<'MA when 'MA :> Ast.MatchArm> (p: PackratParser<Token, 'MA>) : PackratParser<Token, Ast.MatchArm> =
        p |>> fun arm -> arm :> Ast.MatchArm
    let asFnDecl (p: PackratParser<Token, Ast.Decl.Fn>) : PackratParser<Token, Ast.Decl> =
        p |>> fun f -> f :> Ast.Decl

    let block<'A> (opener: PackratParser<Token, Token>) (body: PackratParser<Token, 'A>): PackratParser<Token, 'A> =
        Delay (fun () -> fun input pos ->
            match opener input pos with
                | Success (token, nextPos) -> 
                    // opener と同行にある最左トークンの列をオフサイド基準とする。
                    // opener が行の途中（例: `fn foo = do ...` の `do`）にある場合でも、
                    // ボディが正しく可視となるよう行頭から探索する。
                    let offsideLine = [| 0 .. token.span.left.Column |] |> Array.find (fun i -> (input.get { Line = token.span.left.Line; Column = i }).IsSome)
                    body (BlockInput (input, { Line = token.span.left.Line; Column = offsideLine })) nextPos
                | Failure (reason, span) -> Failure (reason, span)
        )

    // opener トークン自身の列をオフサイド基準にするブロックコンビネーター。
    // `block` が「同行の最左トークン」の列を使うのとは異なり、opener 自身の列を使う汎用の変形。
    // ifBranch/ifElseBranch/matchArm で使用: `if | a => 1 | else => 2` のようなインライン形式で
    // `|` が `if` と同一行にある場合、opener(`|`)の列をオフサイドとすることで
    // 後続行の同列 `|` ブランチ区切りがボディスコープから不可視になる。
    let blockAtOpener<'A> (opener: PackratParser<Token, Token>) (body: PackratParser<Token, 'A>): PackratParser<Token, 'A> =
        Delay (fun () -> fun input pos ->
            match opener input pos with
                | Success (token, nextPos) ->
                    body (BlockInput (input, { Line = token.span.left.Line; Column = token.span.left.Column })) nextPos
                | Failure (reason, span) -> Failure (reason, span)
        )

    // `|` は if/match ブランチ区切り専用で二項中置演算子ではない。infixOp から除外する。
    let infixOp prec : PackratParser<Token, Token.Symbol> =
        AcceptMatch (fun t ->
            match t with
            | :? Token.Symbol as sym when sym.precedence = prec && sym.str <> "|" -> Some(sym)
            | _ -> None)

    let tid: PackratParser<Token, Token.Id> = AcceptMatch (fun t -> match t with :? Token.Id as id -> Some(id) | _ -> None)
    let nonLegacyThisTid: PackratParser<Token, Token.Id> = AcceptMatch (fun t -> match t with :? Token.Id as id when id.str <> "this" -> Some(id) | _ -> None)
    let selfKeywordId: PackratParser<Token, Token.Keyword> = AcceptMatch (fun t -> match t with :? Token.Keyword as kw when kw.str = "self" -> Some(kw) | _ -> None)
    let valueIdent: PackratParser<Token, string * Span> =
        (tid |>> fun id -> id.str, id.span)
        <|> (selfKeywordId |>> fun kw -> kw.str, kw.span)

    let keyword kw: PackratParser<Token, Token.Keyword> = AcceptMatch (fun t -> match t with :? Token.Keyword as st when st.str = kw -> Some(st) | _ -> None)
    let delim d: PackratParser<Token, Token.Delim> = AcceptMatch (fun t -> match t with :? Token.Delim as st when st.char = d -> Some(st) | _ -> None)
    let symbol sym: PackratParser<Token, Token.Symbol> = AcceptMatch (fun t -> match t with :? Token.Symbol as st when st.str = sym -> Some(st) | _ -> None)

    // 式
    let id =
        valueIdent
        |>> fun (name, span) -> Ast.Expr.Id (name, span)
    let int: PackratParser<Token, Ast.Expr.Int> = AcceptMatch (fun t -> match t with :? Token.Int as st -> Some(Ast.Expr.Int(st.value, st.span)) | _ -> None)
    let float: PackratParser<Token, Ast.Expr.Float> = AcceptMatch (fun t -> match t with :? Token.Float as st -> Some(Ast.Expr.Float(st.value, st.span)) | _ -> None)
    let double: PackratParser<Token, Ast.Expr.Double> = AcceptMatch (fun t -> match t with :? Token.Double as st -> Some(Ast.Expr.Double(st.value, st.span)) | _ -> None)
    let str: PackratParser<Token, Ast.Expr.String> = AcceptMatch (fun t -> match t with :? Token.String as st -> Some(Ast.Expr.String(st.value, st.span)) | _ -> None)
    /// `true` / `false` キーワードを Bool リテラルとして解析する。
    let bool: PackratParser<Token, Ast.Expr.Bool> =
        AcceptMatch (fun t ->
            match t with
            | :? Token.Keyword as kw when kw.str = "True"  -> Some(Ast.Expr.Bool(true,  kw.span))
            | :? Token.Keyword as kw when kw.str = "False" -> Some(Ast.Expr.Bool(false, kw.span))
            | _ -> None)
    let unit: PackratParser<Token, Ast.Expr> = delim '(' <&> delim ')' |>> fun (l, r) -> Ast.Expr.Unit({ left = l.span.left; right = r.span.right })

    // 各パーサを安定した値（let rec ... and ...）として定義し、Delay で包む。
    // Delay は初回呼び出し時に f() を一度だけ評価してパーサを取得し、
    // その結果を PackratParser.fs 内部の Memo でメモ化する。
    // 相互再帰は let rec ... and ... で処理される。Delay のクロージャ内では
    // 他のパーサを即座に評価せず、解析呼び出し時まで遅延させるため、
    // 全バインディング初期化後の値として正しく参照される。
    let rec paren: PackratParser<Token, Ast.Expr> =
        Delay (fun () -> fun input pos ->
            match (delim '(') input pos with
            | Failure (reason, span) -> Failure (reason, span)
            | Success (l, afterOpenPos) ->
                match (delim ')') input afterOpenPos with
                | Success (r, nextPos) ->
                    Success (Ast.Expr.Unit({ left = l.span.left; right = r.span.right }) :> Ast.Expr, nextPos)
                | Failure _ ->
                    match expr input afterOpenPos with
                    | Success (innerExpr, afterExprPos) ->
                        match (delim ')') input afterExprPos with
                        | Success (_, nextPos) -> Success (innerExpr, nextPos)
                        | Failure (reason, span) -> Failure (reason, span)
                    | Failure (reason, span) -> Failure (reason, span))
    and doExpr: PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            block (asToken (keyword "do")) (Once ((Many1 stmt |>> fun stmts -> Ast.Expr.Block(stmts, { left = stmts.Head.span.left; right = (List.last stmts).span.right })) <& Eoi) (fun (msg, span) -> Ast.Expr.Error(msg, span) :> Ast.Expr)))

    // if式 / if文共通ブランチパーサー
    // 条件ブランチ: | condition => body
    and ifBranch: PackratParser<Token, Ast.IfBranch> =
        Delay (fun () ->
            blockAtOpener (asToken (symbol "|")) (expr <& keyword "=>" <&> (Once (Many1 stmt |>> fun (stmts) -> Ast.Expr.Block(stmts, { left = stmts.Head.span.left; right = (List.last stmts).span.right }) :> Ast.Expr) (fun (msg, span) -> Ast.Expr.Error(msg, span))) |>> fun (cond, body) -> Ast.IfBranch.Then(cond, body, { left = cond.span.left; right = body.span.right })))

    // else ブランチ: | else => body
    and ifElseBranch: PackratParser<Token, Ast.IfBranch> =
        Delay (fun () ->
            blockAtOpener (asToken (symbol "|")) (keyword "else" &> keyword "=>" &> (Once (Many1 stmt |>> fun (stmts) -> Ast.Expr.Block(stmts, { left = stmts.Head.span.left; right = (List.last stmts).span.right }) :> Ast.Expr) (fun (msg, span) -> Ast.Expr.Error(msg, span))) |>> fun (body) -> Ast.IfBranch.Else(body, body.span)))

    // if式: else 必須
    and ifExpr: PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            keyword "if" <&> (Many1 ifBranch <&> ifElseBranch)
            |>> fun (ifKw, (branches, elseBranch)) ->
                Ast.Expr.If(branches @ [elseBranch], { left = ifKw.span.left; right = elseBranch.span.right }) :> Ast.Expr)

    // 項
    and dataInitField: PackratParser<Token, Ast.DataInitField> =
        Delay (fun () ->
            tid <& symbol "=" <&> expr
            |>> fun (fieldId, fieldValue) ->
                Ast.DataInitField.Field(fieldId.str, fieldValue, { left = fieldId.span.left; right = fieldValue.span.right }) :> Ast.DataInitField)

    // `{ field = value, ... }` 形式の連想配列リテラル（型名を伴わない）。
    // struct 初期化は `{...} TypeName.` の dot-call として、term2 で正規化される。
    and recordLitExpr: PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            delim '{' <&> SepBy1 dataInitField (delim ',') <&> delim '}'
            |>> fun ((openBrace, fields), closeBrace) ->
                Ast.Expr.RecordLit(fields, { left = openBrace.span.left; right = closeBrace.span.right }) :> Ast.Expr)

    // `EnumType'CaseName { field = value, ... }` 形式の enum case 初期化式を解析する。
    and enumInitExpr: PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            tid <& delim '\'' <&> tid <& delim '{' <&> SepBy1 dataInitField (delim ',') <&> delim '}'
            |>> fun (((typeId, caseId), fields), closeBrace) ->
                Ast.Expr.EnumInit(typeId.str, caseId.str, fields, { left = typeId.span.left; right = closeBrace.span.right }) :> Ast.Expr)

    and patternField: PackratParser<Token, Ast.PatternField> =
        Delay (fun () ->
            tid
            |>> fun id ->
                Ast.PatternField.Named(id.str, id.span) :> Ast.PatternField)

    and enumPatternFieldList: PackratParser<Token, Ast.PatternField list * bool> =
        Delay (fun () ->
            ((patternField <&> Many (delim ',' &> patternField) <&> Optional (delim ',' &> symbol ".."))
             |>> fun ((firstField, restFields), hasRestOpt) -> firstField :: restFields, hasRestOpt.IsSome)
            <|> (symbol ".." |>> fun _ -> [], true))

    and enumPattern: PackratParser<Token, Ast.Pattern> =
        Delay (fun () ->
            tid <& delim '\'' <&> tid <&> (
                // `{ field1, field2, .. }` 形式（named fields）
                (delim '{' &> enumPatternFieldList <&> delim '}'
                 |>> fun ((fields, hasRest), closeBrace) ->
                     Some(fields, hasRest, closeBrace.span.right))
                // `identifier` 形式（positional binding）: `| Type'Case varName ->`
                <|> (tid
                     |>> fun idToken ->
                         Some([Ast.PatternField.Positional(idToken.str, idToken.span) :> Ast.PatternField], false, idToken.span.right))
                // フィールドなし
                <|> (Delay (fun () -> fun _ pos -> Success(None, pos))))
            |>> fun ((typeId, caseId), fieldSpecOpt) ->
                let fields, hasRest, rightSpanRight =
                    match fieldSpecOpt with
                    | Some (fields, hasRest, right) -> fields, hasRest, right
                    | None -> [], false, caseId.span.right
                Ast.Pattern.Enum(typeId.str, caseId.str, fields, hasRest, { left = typeId.span.left; right = rightSpanRight }) :> Ast.Pattern)

    and matchArm: PackratParser<Token, Ast.MatchArm> =
        Delay (fun () ->
            blockAtOpener (asToken (symbol "|")) (enumPattern <& keyword "->" <&>
                (Once (Many1 stmt |>> fun stmts -> Ast.Expr.Block(stmts, { left = stmts.Head.span.left; right = (List.last stmts).span.right }) :> Ast.Expr) (fun (msg, span) -> Ast.Expr.Error(msg, span) :> Ast.Expr))
            |>> fun (pattern, body) ->
                Ast.MatchArm.Arm(pattern, body, { left = pattern.span.left; right = body.span.right }) :> Ast.MatchArm))

    and matchExpr: PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            keyword "match" <&> expr <&> Many1 matchArm
            |>> fun ((matchKw, scrutinee), arms) ->
                Ast.Expr.Match(scrutinee, arms, { left = matchKw.span.left; right = (List.last arms).span.right }) :> Ast.Expr)

    and factor: PackratParser<Token, Ast.Expr> =
        Delay (fun () -> paren <|> ifExpr <|> matchExpr <|> doExpr <|> enumInitExpr <|> recordLitExpr <|> (asExpr id) <|> (asExpr float) <|> (asExpr double) <|> (asExpr int) <|> (asExpr str) <|> (asExpr bool))

    and postfixMemberAccess: PackratParser<Token, (Ast.Expr -> Ast.Expr)> =
        Delay (fun () -> fun input pos ->
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
                         afterQuotePos))

    // 型引数付き呼び出しの postfix を受理する（例: receiver<Application>.）。
    // `<` は比較演算子でもあるため、`<...>` の直後に呼び出し `.` が続く場合のみジェネリック適用とみなす。
    // （ジェネリック適用は常に `<T>.` の形になる。実引数は callee の前に置かれるため。）
    // `Not (Not (symbol "."))` は `.` の非消費先読み（正の先読み）。
    and postfixGenericApply: PackratParser<Token, (Ast.Expr -> Ast.Expr)> =
        Delay (fun () ->
            (symbol "<" &> SepBy1 typeExpr (delim ',') <&> symbol ">") <& Not (Not (symbol ".")) |>> fun (typeArgs, closeBracket) ->
                fun receiver ->
                    Ast.Expr.GenericApply(receiver, typeArgs, { left = receiver.span.left; right = closeBracket.span.right }) :> Ast.Expr)

    // 添字アクセスの postfix を受理する（例: receiver[index]）。索引は任意の式。
    and postfixIndexAccess: PackratParser<Token, (Ast.Expr -> Ast.Expr)> =
        Delay (fun () ->
            delim '[' &> expr <&> delim ']' |>> fun (index, closeBracket) ->
                fun receiver ->
                    Ast.Expr.IndexAccess(receiver, index, { left = receiver.span.left; right = closeBracket.span.right }) :> Ast.Expr)

    and postfixExpr: PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            factor <&> Many (postfixGenericApply <|> postfixIndexAccess <|> postfixMemberAccess)
            |>> fun (headExpr, postfixes) -> List.fold (fun current applyPostfix -> applyPostfix current) headExpr postfixes)

    // 代入左辺として許可される式を解析する。
    // 許可: 識別子、アポストロフィによるメンバーアクセス連鎖（例: a'b'c）、添字アクセス（例: a[i]、a'b[i]）。
    // 非許可: 呼び出し結果、リテラル、型引数適用。
    and assignLValueExpr: PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            (asExpr id) <&> Many (postfixMemberAccess <|> postfixIndexAccess)
            |>> fun (headExpr, postfixes) -> List.fold (fun current applyPostfix -> applyPostfix current) headExpr postfixes)

    // 単項マイナスを解析し、AST の既存 Apply 形状へ正規化する。
    // - 負の数値リテラルは literal 値へ直接畳み込む。
    // - それ以外の `-expr` は単項適用 `-(expr)` 形へ変換する。
    //   （バイナリ `-` から独立した単項オーバーロードとして解決される。）
    and unaryTerm: PackratParser<Token, Ast.Expr> =
        Delay (fun () -> fun input pos ->
            match (symbol "-") input pos with
            | Success (minusToken, afterMinusPos) ->
                match unaryTerm input afterMinusPos with
                | Success (operandExpr, nextPos) ->
                    match operandExpr with
                    | :? Ast.Expr.Int as intExpr ->
                        Success (Ast.Expr.Int(-intExpr.value, { left = minusToken.span.left; right = intExpr.span.right }) :> Ast.Expr, nextPos)
                    | :? Ast.Expr.Float as floatExpr ->
                        Success (Ast.Expr.Float(-floatExpr.value, { left = minusToken.span.left; right = floatExpr.span.right }) :> Ast.Expr, nextPos)
                    | :? Ast.Expr.Double as doubleExpr ->
                        Success (Ast.Expr.Double(-doubleExpr.value, { left = minusToken.span.left; right = doubleExpr.span.right }) :> Ast.Expr, nextPos)
                    | _ ->
                        let negatedExpr =
                            Ast.Expr.Apply(
                                Ast.Expr.Id("-", minusToken.span) :> Ast.Expr,
                                [ operandExpr ],
                                { left = minusToken.span.left; right = operandExpr.span.right }) :> Ast.Expr
                        Success (negatedExpr, nextPos)
                | Failure (reason, span) -> Failure (reason, span)
            | Failure _ ->
                postfixExpr input pos)

    // 呼び出し式の項。添字アクセスは postfixExpr の `[i]` で扱う。
    and term1: PackratParser<Token, Ast.Expr> =
        Delay (fun () -> unaryTerm)

    // Dot-only 呼び出し式を解析する。
    // 仕様:
    // - `f.` は 0 引数呼び出し。
    // - `x .` は callable 式 `x` の 0 引数呼び出し。
    // - `x f.` は `f(x)` に正規化される。
    // - `a b f.` は `f(a, b)` に正規化される。
    // - `x f. g.` は `g(f(x))` に正規化される。
    // - `x f` / `a b f` は parse error（callee の直後に `.` 必須）。
    and term2: PackratParser<Token, Ast.Expr> =
        Delay (fun () -> fun input pos ->
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
                            | Some (:? Token.Symbol as sym) when sym.str <> "." ->
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
                loop head afterHeadPos)

    // `await Expr` を解析する。dot-call チェーン全体（term2）をオペランドとして取り込むため、
    // term2 と binop の間のレイヤーに位置する。
    // `await base'LoadContent.` は `Await(Apply(MemberAccess(base, LoadContent), []))` となる。
    // バイナリ演算子よりは結合が緩いため、`await foo. + 1` は `(await foo.) + 1` と解釈される。
    // `async fn` 本体内でのみ使用可能で、それ以外の文脈での使用は Analyze フェーズで診断される。
    and awaitExpr: PackratParser<Token, Ast.Expr> =
        Delay (fun () -> fun input pos ->
            match (keyword "await") input pos with
            | Success (kw, afterKwPos) ->
                match awaitExpr input afterKwPos with
                | Success (operand, nextPos) ->
                    Success
                        (Ast.Expr.Await(operand, { left = kw.span.left; right = operand.span.right }) :> Ast.Expr,
                         nextPos)
                | Failure (reason, span) -> Failure (reason, span)
            | Failure _ ->
                term2 input pos)

    // 二項演算
    and binopExpr: PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            List.fold (fun acc prec ->
                let op = infixOp prec
                acc <&> Many (op <&> acc) |>> fun (left, rest) ->
                    rest
                    |> List.fold (fun current (op, right) ->
                        Ast.Expr.Apply (Ast.Expr.Id(op.str, op.span), [current; right], { left = current.span.left; right = right.span.right }) :> Ast.Expr) left
            ) awaitExpr [9 .. -1 .. 0])

    // lambda 引数名の重複を検証し、最初に見つかった重複名を返す。
    and private tryFindDuplicateArgName (args: Ast.FnArg list) : string option =
        let argNames =
            args
            |> List.choose (fun arg ->
                match arg with
                | :? Ast.FnArg.Named as namedArg -> Some namedArg.name
                | :? Ast.FnArg.Inferred as inferredArg -> Some inferredArg.name
                | _ -> None)
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
    and lambdaArg: PackratParser<Token, Ast.FnArg> =
        Delay (fun () -> fnArgUnit <|> fnArgNamed <|> (valueIdent |>> fun (name, span) -> Ast.FnArg.Inferred(name, span) :> Ast.FnArg))

    and lambdaExpr: PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            block (asToken (keyword "fn")) (Once (Many lambdaArg <& keyword "->" <&> fnBodyExpr |>> fun (args, body) -> 
                // Unit引数を除外（0引数関数として扱う）
                let nonUnitArgs = 
                    args |> List.filter (fun arg -> 
                        match arg with
                        | :? Ast.FnArg.Unit -> false
                        | _ -> true)

                // 空引数チェック（元の引数リストが明示的なUnitのみの場合は許可）
                let hasOnlyUnit = args.Length = 1 && (args.[0] :? Ast.FnArg.Unit)
                if nonUnitArgs.IsEmpty && not hasOnlyUnit then
                    Ast.Expr.Error("Lambda parameter list is empty", body.span) :> Ast.Expr
                else
                    // 重複チェック
                    match tryFindDuplicateArgName nonUnitArgs with
                    | Some dupName ->
                        Ast.Expr.Error(sprintf "Duplicate lambda parameter '%s'" dupName, body.span) :> Ast.Expr
                    | None ->
                        let leftSpan = 
                            match nonUnitArgs with
                            | [] -> body.span.left
                            | head :: _ -> head.span.left
                        Ast.Expr.Lambda (nonUnitArgs, body, { left = leftSpan; right = body.span.right }) :> Ast.Expr) (fun (msg, span) -> Ast.Expr.Error(msg, span) :> Ast.Expr)))

    // 後置の型注釈（ascription）`binopExpr : typeExpr` を解析する。
    // `.` ドット呼び出しは term2 層（binopExpr より下）で処理されるため、
    // `List.` 全体を取り込むには binop より上で注釈を付ける必要がある。
    // よって注釈は二項演算より緩く結合し、`a + b : T` は `(a + b) : T` となる。
    and ascriptionExpr: PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            binopExpr <&> Optional (delim ':' &> typeExpr)
            |>> fun (e, typeOpt) ->
                match typeOpt with
                | None -> e
                | Some te -> Ast.Expr.TypeAscription(e, te, { left = e.span.left; right = te.span.right }) :> Ast.Expr)

    and expr: PackratParser<Token, Ast.Expr> =
        Delay (fun () -> lambdaExpr <|> ascriptionExpr)

    // `| else -> stmt...` を解析する（let-else / var-else の else ブランチ）。
    // blockAtOpener で `|` の列をオフサイド基準とし、ボディは `|` より右に字下げする。
    and letElseElseBranch: PackratParser<Token, Ast.Stmt list> =
        Delay (fun () ->
            blockAtOpener (asToken (symbol "|")) (
                keyword "else" &> keyword "->" &>
                Once (Many1 stmt) (fun (msg, span) -> [ Ast.Stmt.Error(msg, span) :> Ast.Stmt ])))

    // `let <enumPattern> = <expr> <letElseElseBranch>` を解析する。
    and letElseStmt: PackratParser<Token, Ast.Stmt> =
        Delay (fun () ->
            block (asToken (keyword "let")) (enumPattern <& symbol "=" <&> expr)
            <&> letElseElseBranch
            |>> fun ((pattern, value), elseBranch) ->
                let rightSpan = if elseBranch.IsEmpty then value.span.right else (List.last elseBranch).span.right
                Ast.Stmt.LetElse(pattern, value, elseBranch, { left = pattern.span.left; right = rightSpan }) :> Ast.Stmt)

    // `var <enumPattern> = <expr> <letElseElseBranch>` を解析する。
    and varElseStmt: PackratParser<Token, Ast.Stmt> =
        Delay (fun () ->
            block (asToken (keyword "var")) (enumPattern <& symbol "=" <&> expr)
            <&> letElseElseBranch
            |>> fun ((pattern, value), elseBranch) ->
                let rightSpan = if elseBranch.IsEmpty then value.span.right else (List.last elseBranch).span.right
                Ast.Stmt.VarElse(pattern, value, elseBranch, { left = pattern.span.left; right = rightSpan }) :> Ast.Stmt)

    // 文
    and letStmt: PackratParser<Token, Ast.Stmt> =
        Delay (fun () ->
            block (asToken (keyword "let")) (Once (tid <&> Optional (delim ':' &> typeExpr) <& symbol "=" <&> expr |>> fun ((id, typeAnnOpt), rhs) -> Ast.Stmt.Let (id.str, typeAnnOpt, rhs, { left = id.span.left; right = rhs.span.right})) (fun (msg, span) -> Ast.Stmt.Error(msg, span) :> Ast.Stmt)))

    and varStmt: PackratParser<Token, Ast.Stmt> =
        Delay (fun () ->
            block (asToken (keyword "var")) (Once (tid <&> Optional (delim ':' &> typeExpr) <& symbol "=" <&> expr |>> fun ((id, typeAnnOpt), rhs) -> Ast.Stmt.Var (id.str, typeAnnOpt, rhs, { left = id.span.left; right = rhs.span.right})) (fun (msg, span) -> Ast.Stmt.Error(msg, span) :> Ast.Stmt)))

    and assignStmt: PackratParser<Token, Ast.Stmt> =
        Delay (fun () ->
            (assignLValueExpr <& symbol "=" <&> expr
             |>> fun ((target: Ast.Expr), (rhs: Ast.Expr)) -> Ast.Stmt.Assign(target, rhs, { left = target.span.left; right = rhs.span.right }) :> Ast.Stmt)
            <|>
            (assignLValueExpr <& symbol "+=" <&> expr
             |>> fun ((target: Ast.Expr), (rhs: Ast.Expr)) -> Ast.Stmt.CompoundAssign(Ast.Stmt.CompoundAssignOp.Add, target, rhs, { left = target.span.left; right = rhs.span.right }) :> Ast.Stmt)
            <|>
            (assignLValueExpr <& symbol "-=" <&> expr
             |>> fun ((target: Ast.Expr), (rhs: Ast.Expr)) -> Ast.Stmt.CompoundAssign(Ast.Stmt.CompoundAssignOp.Sub, target, rhs, { left = target.span.left; right = rhs.span.right }) :> Ast.Stmt)
            <|>
            (assignLValueExpr <& symbol "*=" <&> expr
             |>> fun ((target: Ast.Expr), (rhs: Ast.Expr)) -> Ast.Stmt.CompoundAssign(Ast.Stmt.CompoundAssignOp.Mul, target, rhs, { left = target.span.left; right = rhs.span.right }) :> Ast.Stmt)
            <|>
            (assignLValueExpr <& symbol "/=" <&> expr
             |>> fun ((target: Ast.Expr), (rhs: Ast.Expr)) -> Ast.Stmt.CompoundAssign(Ast.Stmt.CompoundAssignOp.Div, target, rhs, { left = target.span.left; right = rhs.span.right }) :> Ast.Stmt))

    and exprStmt: PackratParser<Token, Ast.Stmt> =
        Delay (fun () -> expr |>> fun e -> Ast.Stmt.ExprStmt (e, e.span) :> Ast.Stmt)

    and returnStmt: PackratParser<Token, Ast.Stmt> =
        Delay (fun () -> fun input pos ->
            match keyword "return" input pos with
            | Failure (msg, span) -> Failure (msg, span)
            | Success (kw, afterKwPos) ->
                let lineInput = LineInput(input, kw.span.left.Line) :> Input<Token>
                match expr lineInput afterKwPos with
                | Success (e, nextPos) ->
                    Success (Ast.Stmt.Return (e, { left = kw.span.left; right = e.span.right }) :> Ast.Stmt, nextPos)
                | Failure _ ->
                    let unitExpr = Ast.Expr.Unit(kw.span) :> Ast.Expr
                    Success (Ast.Stmt.Return (unitExpr, kw.span) :> Ast.Stmt, afterKwPos))

    and breakStmt: PackratParser<Token, Ast.Stmt> =
        Delay (fun () -> keyword "break" |>> fun kw -> Ast.Stmt.Break (kw.span) :> Ast.Stmt)

    and continueStmt: PackratParser<Token, Ast.Stmt> =
        Delay (fun () -> keyword "continue" |>> fun kw -> Ast.Stmt.Continue (kw.span) :> Ast.Stmt)

    and whileStmt: PackratParser<Token, Ast.Stmt> =
        Delay (fun () -> fun input pos ->
            match (keyword "while") input pos with
            | Success (whileKw, afterWhilePos) ->
                let bodyInput = BlockInput(input, whileKw.span.left) :> Input<Token>
                let condInput = LineInput(bodyInput, whileKw.span.left.Line) :> Input<Token>
                match expr condInput afterWhilePos with
                | Success (cond, afterCondPos) ->
                    match Many1 stmt bodyInput afterCondPos with
                    | Success (bodyStmts, nextPos) ->
                        Success (Ast.Stmt.While(cond, bodyStmts, { left = whileKw.span.left; right = (List.last bodyStmts).span.right }) :> Ast.Stmt, nextPos)
                    | Failure (msg, span) ->
                        Success (Ast.Stmt.Error(msg, span) :> Ast.Stmt, afterCondPos)
                | Failure (msg, span) ->
                    Success (Ast.Stmt.Error(msg, span) :> Ast.Stmt, afterWhilePos)
            | Failure (reason, span) -> Failure (reason, span))

    and forStmt: PackratParser<Token, Ast.Stmt> =
        Delay (fun () -> fun input pos ->
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
            | Failure (reason, span) -> Failure (reason, span))

    // if文: else オプション
    // if文: else オプション
    and ifStmt: PackratParser<Token, Ast.Stmt> =
        Delay (fun () ->
            keyword "if" <&> (Many1 ifBranch <&> Optional ifElseBranch)
            |>> fun (ifKw, (branches, elseBranchOpt)) ->
                let allBranches = branches @ (match elseBranchOpt with Some e -> [e] | None -> [])
                let span = { left = ifKw.span.left; right = (List.last allBranches).span.right }
                Ast.Stmt.If(allBranches, span) :> Ast.Stmt)

    // else あり `if` はまず exprStmt (ifExpr) として試みる。else なしの場合のみ ifStmt にフォールバック。
    // letElseStmt / varElseStmt は enumPattern（`Type'Case`）で始まるため letStmt / varStmt と衝突しない。
    and stmt: PackratParser<Token, Ast.Stmt> =
        Delay (fun () -> letElseStmt <|> varElseStmt <|> letStmt <|> varStmt <|> returnStmt <|> breakStmt <|> continueStmt <|> whileStmt <|> forStmt <|> assignStmt <|> exprStmt <|> ifStmt)

    and typeExprUnit: PackratParser<Token, Ast.TypeExpr> =
        Delay (fun () ->
            delim '(' <&> delim ')' |>> fun (l,r) -> Ast.TypeExpr.Unit ({ left = l.span.left; right = r.span.right }) :> Ast.TypeExpr)

    and typeExprId: PackratParser<Token, Ast.TypeExpr> =
        Delay (fun () -> tid |>> fun id -> Ast.TypeExpr.Id (id.str, id.span) :> Ast.TypeExpr)
        
    and typeExprAtom: PackratParser<Token, Ast.TypeExpr> =
        Delay (fun () -> (asTypeExpr typeExprUnit) <|> (asTypeExpr typeExprId))

    // 空白区切りの型適用（例: Array String）を左結合で畳み込む。
    and typeExprApply: PackratParser<Token, Ast.TypeExpr> =
        Delay (fun () ->
            Many1 typeExprAtom
            |>> fun typeExprs ->
                match typeExprs with
                | [] ->
                    // Many1 により通常は到達しないが、網羅性警告を避けるため防御的に失敗させる。
                    failwith "type expression list must not be empty"
                | head :: [] -> head
                | head :: tail ->
                    Ast.TypeExpr.Apply(head, tail, { left = head.span.left; right = (List.last tail).span.right }) :> Ast.TypeExpr)

    // 関数型（例: Int -> Int）を右結合でパースする。
    // typeExpr := typeExprApply ('->' typeExpr)?
    and typeExpr: PackratParser<Token, Ast.TypeExpr> =
        Delay (fun () ->
            typeExprApply <&> Optional (keyword "->" &> typeExpr)
            |>> fun (argType, retOpt) ->
                match retOpt with
                | None -> argType
                | Some retType ->
                    Ast.TypeExpr.Arrow(argType, retType, { left = argType.span.left; right = retType.span.right }) :> Ast.TypeExpr)

    // struct 宣言のフィールド: `val name: Type` または `var name: Type`。
    // val/var の欠落はパースエラーとし、可変性アノテーションを必須にする。
    and structField: PackratParser<Token, Ast.DataItem.Field> =
        Delay (fun () ->
            ((keyword "val" |>> fun kw -> kw, false) <|> (keyword "var" |>> fun kw -> kw, true))
            <&> tid <& delim ':' <&> typeExpr
            |>> fun (((kw, isMutable), id), typeExpr) ->
                Ast.DataItem.Field(id.str, typeExpr, isMutable, { left = kw.span.left; right = typeExpr.span.right }))

    and structFieldItem: PackratParser<Token, Ast.DataItem> =
        Delay (fun () -> asDataItem structField)

    and enumCaseField: PackratParser<Token, Ast.EnumCase.Field> =
        Delay (fun () ->
            tid <& delim ':' <&> typeExpr
            |>> fun (id, typeExpr) -> Ast.EnumCase.Field(id.str, typeExpr, { left = id.span.left; right = typeExpr.span.right }))

    and enumCase: PackratParser<Token, Ast.EnumCase> =
        Delay (fun () ->
            blockAtOpener (asToken (symbol "|"))
                (tid <&> Optional (delim '{' &> SepBy1 enumCaseField (delim ',') <&> delim '}')
                 |>> fun (caseId, fieldsOpt) ->
                     let fields =
                         fieldsOpt
                         |> Option.map fst
                         |> Option.defaultValue []
                     let rightSpan =
                         match fieldsOpt with
                         | Some (_, closeBrace) -> closeBrace.span.right
                         | None -> caseId.span.right
                     Ast.EnumCase.Case(caseId.str, fields, { left = caseId.span.left; right = rightSpan }) :> Ast.EnumCase))

    and dataDecl: PackratParser<Token, Ast.Decl> =
        Delay (fun () ->
            block (asToken (keyword "struct"))
                (Once
                    (tid <&> Many tid <&> Many1 structFieldItem
                     |>> fun ((id, typeParams), items) ->
                         let typeParamNames = typeParams |> List.map (fun t -> t.str)
                         let rightSpan =
                             match items |> List.tryLast with
                             | Some lastItem -> lastItem.span.right
                             | None -> id.span.right
                         Ast.Decl.Data(id.str, typeParamNames, items, { left = id.span.left; right = rightSpan }) :> Ast.Decl)
                    (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl)))

    and enumDecl: PackratParser<Token, Ast.Decl> =
        Delay (fun () ->
            block (asToken (keyword "enum"))
                (Once
                    (tid <&> Many tid <&> Many1 enumCase
                     |>> fun ((id, typeParams), cases) ->
                         let typeParamNames = typeParams |> List.map (fun t -> t.str)
                         let rightSpan =
                             match cases |> List.tryLast with
                             | Some lastCase -> lastCase.span.right
                             | None -> id.span.right
                         Ast.Decl.Enum(id.str, typeParamNames, cases, { left = id.span.left; right = rightSpan }) :> Ast.Decl)
                    (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl)))

    // インポート宣言（`public import Foo` または `import Foo`）
    and importDecl: PackratParser<Token, Ast.Decl> =
        Delay (fun () ->
            Optional (asToken (keyword "public"))
            >>= fun publicTokenOpt ->
                let isPublic = Option.isSome publicTokenOpt
                block (asToken (keyword "import")) (Once (SepBy1 tid (delim '\'') |>> fun ids -> Ast.Decl.Import (ids |> List.map (fun id -> id.str), isPublic, { left = ids.Head.span.left; right = (List.last ids).span.right })) (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl)))

    // 関数宣言
    and fnArgNamed: PackratParser<Token, Ast.FnArg> =
        Delay (fun () ->
            delim '(' &> nonLegacyThisTid <& delim ':' <&> typeExpr <& delim ')' |>> fun ((id, typeExpr)) -> Ast.FnArg.Named(id.str, typeExpr, { left = id.span.left; right = typeExpr.span.right }) :> Ast.FnArg)

    and fnArgUnit: PackratParser<Token, Ast.FnArg> =
        Delay (fun () ->
            delim '(' <&> delim ')' |>> fun (l,r) -> Ast.FnArg.Unit { left = l.span.left; right = r.span.right } :> Ast.FnArg)

    and fnArg: PackratParser<Token, Ast.FnArg> =
        Delay (fun () -> fnArgUnit <|> fnArgNamed)

    and selfReceiverArg: PackratParser<Token, Ast.FnArg> =
        Delay (fun () ->
            keyword "self" |>> fun selfKeyword -> Ast.FnArg.Inferred("self", selfKeyword.span) :> Ast.FnArg)

    and methodArgList: PackratParser<Token, Ast.FnArg list> =
        Delay (fun () ->
            Optional selfReceiverArg <&> Many fnArg
            |>> fun (selfOpt, args) -> (Option.toList selfOpt) @ args)

    and fnBodyExpr: PackratParser<Token, Ast.Expr> =
        Delay (fun () ->
            Many1 stmt |>> fun stmts -> Ast.Expr.Block(stmts, { left = stmts.Head.span.left; right = (List.last stmts).span.right }) :> Ast.Expr)

    // 共通ヘルパ: `fn` キーワード以降のシグネチャ＋本体を解析し、Decl.Fn を構築する。
    // - シグネチャ（name / args / `:` / 戻り値型）は `fn` と同一行に限定（LineInput）して解析する。
    //   これにより `fn foo: Int\n    x ping.` の body 先頭識別子が typeExpr に取り込まれるのを防ぐ。
    // - 本体は `outermostPos`（fn または override/async など最も左の修飾子）の開始列でオフサイド境界を引き、
    //   必ず次の行から始まる Many1 stmt として解析する。同一行に本体を書いた場合はパースエラー。
    and parseFnAfterKeyword
        (input: Input<Token>)
        (afterFnPos: Position)
        (fnKw: Token)
        (outermostPos: Position)
        (argList: PackratParser<Token, Ast.FnArg list>)
        : ParseResult<Ast.Decl.Fn> =
            let bodyInput = BlockInput(input, outermostPos) :> Input<Token>
            let sigInput = LineInput(bodyInput, fnKw.span.left.Line) :> Input<Token>
            match tid sigInput afterFnPos with
            | Failure (msg, span) -> Failure (msg, span)
            | Success (id, afterIdPos) ->
                match argList sigInput afterIdPos with
                | Failure (msg, span) -> Failure (msg, span)
                | Success (args, afterArgsPos) ->
                    match (delim ':') sigInput afterArgsPos with
                    | Failure (msg, span) -> Failure (msg, span)
                    | Success (_, afterColonPos) ->
                        match typeExpr sigInput afterColonPos with
                        | Failure (msg, span) -> Failure (msg, span)
                        | Success (ret, afterRetPos) ->
                            match (Many1 stmt) bodyInput afterRetPos with
                            | Failure (msg, span) -> Failure (msg, span)
                            | Success (stmts, nextPos) ->
                                let firstStmt = stmts.Head
                                if firstStmt.span.left.Line <= ret.span.right.Line then
                                    Failure ("Function body must start on a new line after the signature", firstStmt.span)
                                else
                                    let body =
                                        Ast.Expr.Block(stmts, { left = firstStmt.span.left; right = (List.last stmts).span.right }) :> Ast.Expr
                                    let decl =
                                        Ast.Decl.Fn(id.str, args, ret, body, false, false, { left = id.span.left; right = body.span.right })
                                    Success (decl, nextPos)

    and fnDecl: PackratParser<Token, Ast.Decl> =
        Delay (fun () -> fun input pos ->
            let asyncResult = (Optional (keyword "async")) input pos
            match asyncResult with
            | Failure (msg, span) -> Failure (msg, span)
            | Success (asyncOpt, afterAsyncPos) ->
                match (keyword "fn") input afterAsyncPos with
                | Failure (msg, span) -> Failure (msg, span)
                | Success (fnKw, afterFnPos) ->
                    let outermostPos =
                        match asyncOpt with
                        | Some kw -> kw.span.left
                        | None -> fnKw.span.left
                    let argList = Many fnArg |>> fun args -> args
                    match parseFnAfterKeyword input afterFnPos fnKw outermostPos argList with
                    | Success (decl, nextPos) ->
                        let isAsync = Option.isSome asyncOpt
                        let finalDecl =
                            if isAsync then
                                Ast.Decl.Fn(decl.name, decl.args, decl.ret, decl.body, decl.isOverride, true,
                                    { left = outermostPos; right = decl.span.right }) :> Ast.Decl
                            else
                                decl :> Ast.Decl
                        Success (finalDecl, nextPos)
                    | Failure (msg, span) ->
                        Success (Ast.Decl.Error(msg, span) :> Ast.Decl, afterFnPos))

    // `override` / `async` 修飾子はオプショナル。記述順は `override async fn` を期待する。
    // `override` は `impl A as B` 内のメソッドでのみ意味があり、他文脈での使用は Resolve
    // フェーズでエラーとして検出する（パーサ側では受理する）。
    // `async` は本体で `await` を許可し、戻り値型が Task / Task T であることを Analyze で検査する。
    and implMethodDecl: PackratParser<Token, Ast.Decl.Fn> =
        Delay (fun () -> fun input pos ->
            match (Optional (keyword "override")) input pos with
            | Failure (msg, span) -> Failure (msg, span)
            | Success (overrideOpt, afterOverridePos) ->
                match (Optional (keyword "async")) input afterOverridePos with
                | Failure (msg, span) -> Failure (msg, span)
                | Success (asyncOpt, afterAsyncPos) ->
                    match (keyword "fn") input afterAsyncPos with
                    | Failure (msg, span) -> Failure (msg, span)
                    | Success (fnKw, afterFnPos) ->
                        let outermostPos =
                            match overrideOpt, asyncOpt with
                            | Some kw, _ -> kw.span.left
                            | None, Some kw -> kw.span.left
                            | None, None -> fnKw.span.left
                        match parseFnAfterKeyword input afterFnPos fnKw outermostPos methodArgList with
                        | Success (decl, nextPos) ->
                            let isOverride = Option.isSome overrideOpt
                            let isAsync = Option.isSome asyncOpt
                            let finalDecl =
                                if not isOverride && not isAsync then
                                    decl
                                else
                                    Ast.Decl.Fn(decl.name, decl.args, decl.ret, decl.body, isOverride, isAsync,
                                        { left = outermostPos; right = decl.span.right })
                            Success (finalDecl, nextPos)
                        | Failure (msg, span) ->
                            // 修飾子を含めて消費した範囲を保持しつつ Decl.Fn のエラー版を返し、後続のメソッド解析を継続させる。
                            let errSpan = { left = outermostPos; right = span.right }
                            Success (
                                Ast.Decl.Fn($"error_{errSpan.left.Line}_{errSpan.left.Column}", [], Ast.TypeExpr.Id("Unit", errSpan), Ast.Expr.Error(msg, span), false, false, errSpan),
                                afterFnPos))

    // role 宣言の抽象メソッドシグネチャ（ボディなし）。
    // `fn name args: ret` の形式で、`= body` を持たない。
    and roleFnDecl: PackratParser<Token, Ast.Decl.RoleFn> =
        Delay (fun () ->
            block (asToken (keyword "fn"))
                    (Once
                    (tid <&> methodArgList <& delim ':' <&> typeExpr
                     |>> fun ((id, args), ret) -> Ast.Decl.RoleFn(id.str, args, ret, { left = id.span.left; right = ret.span.right }))
                    (fun (msg, span) ->
                        Ast.Decl.RoleFn($"error_{span.left.Line}_{span.left.Column}", [], Ast.TypeExpr.Id("Unit", span), span))))

    and implDecl: PackratParser<Token, Ast.Decl> =
        // `impl A as B` と `impl A [for Role] [by field]` は相互に排他的な構文。
        // `as` を検出した場合は .NET 継承形式として処理し、`for`/`by` は禁止する。
        // 型パラメータは型名の直後に空白区切りで列挙する（例: `impl Opt T`）。
        Delay (fun () ->
            block (asToken (keyword "impl"))
                (Once
                    ((   // 分岐1: impl A [T...] as B（for/by なし）
                         tid <&> Many tid <& keyword "as" <&> tid <&> Many (asFnDecl implMethodDecl) |>> fun (((typeId, typeParams), asTypeId), methodDecls) ->
                             let typeParamNames = typeParams |> List.map (fun t -> t.str)
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
                             Ast.Decl.Impl(typeId.str, typeParamNames, Some asTypeId.str, None, None, methods, { left = typeId.span.left; right = rightSpan }) :> Ast.Decl)
                     <|> // 分岐2: 既存構文 impl A [T...] [for Role] [by field]
                         (tid <&> Many tid <&> Optional (keyword "for" &> tid) <&> Optional (keyword "by" &> tid) <&> Many (asFnDecl implMethodDecl) |>> fun ((((typeId, typeParams), forTypeIdOpt), byFieldIdOpt), methodDecls) ->
                             let typeParamNames = typeParams |> List.map (fun t -> t.str)
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
                             Ast.Decl.Impl(typeId.str, typeParamNames, None, forTypeName, byFieldName, methods, { left = typeId.span.left; right = rightSpan }) :> Ast.Decl))
                    (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl)))

    and decl: PackratParser<Token, Ast.Decl> =
        Delay (fun () -> dataDecl <|> enumDecl <|> importDecl <|> fnDecl <|> implDecl <|> roleDecl)

    // role 型宣言: `role TypeName` に続くインデントブロック内に抽象メソッドシグネチャを列挙する。
    and roleDecl: PackratParser<Token, Ast.Decl> =
        Delay (fun () ->
            block (asToken (keyword "role"))
                (Once
                    (tid <&> Many roleFnDecl
                     |>> fun (id, methods) ->
                         let rightSpan =
                             match methods |> List.tryLast with
                             | Some lastMethod -> lastMethod.span.right
                             | None -> id.span.right
                         Ast.Decl.Role(id.str, methods, { left = id.span.left; right = rightSpan }) :> Ast.Decl)
                    (fun (msg, span) -> Ast.Decl.Error(msg, span) :> Ast.Decl)))

    // モジュール
    and fileModule: PackratParser<Token, Ast.Module> =
        Delay (fun () -> Many decl <& Eoi |>> fun decls -> Ast.Module (decls))
