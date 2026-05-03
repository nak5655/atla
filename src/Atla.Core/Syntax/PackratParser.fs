namespace Atla.Core.Syntax

open System.Collections.Generic
open System.Runtime.CompilerServices
open Atla.Core.Data
open Atla.Core.Syntax.Data

type PackratParser<'I, 'A when 'I :> HasSpan> = Input<'I> -> Position -> ParseResult<'A>

module Combinators =
    /// パーサの結果を (入力オブジェクト同一性, 位置) のペアでキャッシュするメモ化コンビネータ。
    /// 外側のキーには ConditionalWeakTable を使用し、入力オブジェクトが GC されたときに
    /// 対応するキャッシュエントリが自動的に解放されるようにする。
    /// これにより、BlockInput や LineInput など異なる可視範囲を持つ入力が同一位置に対して
    /// 別々のキャッシュエントリを持ち、正確な結果を返すことが保証される。
    let Memo (p: PackratParser<'I, 'A>) : PackratParser<'I, 'A> =
        let outerCache = ConditionalWeakTable<obj, Dictionary<Position, ParseResult<'A>>>()
        fun input pos ->
            let posCache = outerCache.GetOrCreateValue(input :> obj)
            match posCache.TryGetValue(pos) with
            | true, result -> result
            | _ ->
                let result = p input pos
                posCache.[pos] <- result
                result

    let AcceptIf (predicate: 'I -> bool) : PackratParser<'I, 'I> =
        fun input pos ->
            match input.get pos with
            | Some value when predicate value -> Success (value, input.next pos)
            | Some s -> Failure (sprintf "Unexpected input '%A'" s, { left = pos; right = pos })
            | _ -> Failure ("Unexpected end of input", { left = pos; right = pos })

    let AcceptMatch (predicate: 'I -> 'A option) : PackratParser<'I, 'A> =
        fun input pos ->
            match input.get pos with
            | Some value ->
                match predicate value with
                | Some result -> Success (result, input.next pos)
                | None -> Failure (sprintf "Unexpected input '%A'" value, { left = pos; right = pos })
            | None -> Failure ("Unexpected end of input", { left = pos; right = pos })

    let Fail (message: string) : PackratParser<'I, 'A> =
        fun _ pos -> Failure (message, { left = pos; right = pos })

    let Bind (p: PackratParser<'I, 'A>) (f: 'A -> PackratParser<'I, 'B>) : PackratParser<'I, 'B> =
        fun input pos ->
            match p input pos with
            | Success (value, nextPos) -> f value input nextPos
            | Failure (reason, span) -> Failure (reason, span)

    let (>>=) (p: PackratParser<'I, 'A>) (f: 'A -> PackratParser<'I, 'B>) : PackratParser<'I, 'B> =
        Bind p f

    let Map (p: PackratParser<'I, 'A>) (f: 'A -> 'B) : PackratParser<'I, 'B> =
        fun input pos ->
            match p input pos with
            | Success (value, nextPos) -> Success (f value, nextPos)
            | Failure (reason, span) -> Failure (reason, span)

    let (|>>) (p: PackratParser<'I, 'A>) (f: 'A -> 'B) : PackratParser<'I, 'B> =
        Map p f

    let Or (p1: PackratParser<'I, 'A>) (p2: PackratParser<'I, 'A>) : PackratParser<'I, 'A> =
        fun input pos ->
            match p1 input pos with
            | Success _ as result -> result
            | Failure _ -> p2 input pos

    let (<|>) (p1: PackratParser<'I, 'A>) (p2: PackratParser<'I, 'A>) : PackratParser<'I, 'A> =
        Or p1 p2

    let And (p1: PackratParser<'I, 'A>) (p2: PackratParser<'I, 'B>) : PackratParser<'I, 'A * 'B> =
        fun input pos ->
            match p1 input pos with
            | Success (value1, nextPos1) ->
                match p2 input nextPos1 with
                | Success (value2, nextPos2) -> Success ((value1, value2), nextPos2)
                | Failure (reason, span) -> Failure (reason, span)
            | Failure (reason, span) -> Failure (reason, span)

    let (<&>) (p1: PackratParser<'I, 'A>) (p2: PackratParser<'I, 'B>) : PackratParser<'I, 'A * 'B> =
        And p1 p2
        
    let AndL (p1: PackratParser<'I, 'A>) (p2: PackratParser<'I, 'B>) : PackratParser<'I, 'A> =
        fun input pos ->
            match p1 input pos with
            | Success (value1, nextPos1) ->
                match p2 input nextPos1 with
                | Success (_, nextPos2) -> Success (value1, nextPos2)
                | Failure (reason, span) -> Failure (reason, span)
            | Failure (reason, span) -> Failure (reason, span)

    let (<&) (p1: PackratParser<'I, 'A>) (p2: PackratParser<'I, 'B>) : PackratParser<'I, 'A> =
        AndL p1 p2

    let AndR (p1: PackratParser<'I, 'A>) (p2: PackratParser<'I, 'B>) : PackratParser<'I, 'B> =
        fun input pos ->
            match p1 input pos with
            | Success (_, nextPos1) ->
                match p2 input nextPos1 with
                | Success (value2, nextPos2) -> Success (value2, nextPos2)
                | Failure (reason, span) -> Failure (reason, span)
            | Failure (reason, span) -> Failure (reason, span)

    let (&>) (p1: PackratParser<'I, 'A>) (p2: PackratParser<'I, 'B>) : PackratParser<'I, 'B> =
        AndR p1 p2

    let Not (p: PackratParser<'I, 'A>) : PackratParser<'I, unit> =
        fun input pos ->
            match p input pos with
            | Success _ -> Failure ("Unexpected input", { left = pos; right = pos })
            | Failure _ -> Success ((()), pos)

    let Optional (p: PackratParser<'I, 'A>) : PackratParser<'I, 'A option> =
        fun input pos ->
            match p input pos with
            | Success (value, nextPos) -> Success (Some value, nextPos)
            | Failure _ -> Success (None, pos)

    let Many (p: PackratParser<'I, 'A>) : PackratParser<'I, 'A list> =
        let rec loop input pos acc =
            match p input pos with
            | Success (value, nextPos) -> loop input nextPos (value :: acc)
            | Failure _ -> Success ((List.rev acc), pos)
        fun input pos -> loop input pos []

    let Many1 (p: PackratParser<'I, 'A>) : PackratParser<'I, 'A list> =
        fun input pos ->
            match p input pos with
            | Success (value, nextPos) ->
                let rec loop input pos acc =
                    match p input pos with
                    | Success (value, nextPos) -> loop input nextPos (value :: acc)
                    | Failure _ -> Success (List.rev acc, pos)
                loop input nextPos [value]
            | Failure (reason, span) -> Failure (reason, span)

    let Eoi : PackratParser<'I, unit> =
        fun input pos ->
            match input.get pos with
            | None -> Success ((), pos)
            | Some _ -> Failure ("Expected end of input", { left = pos; right = pos })
            
    let Phrase (es: 'E list) (eq: 'I * 'E -> bool): PackratParser<'I, 'I list> =
        let rec loop (input: Input<'I>) (pos: Position) acc es =
            match es with
            | [] -> Success (List.rev acc, pos)
            | e :: rest ->
                match input.get pos with
                | Some value when eq (value, e) -> loop input (input.next pos) (value :: acc) rest
                | _ -> Failure ("Unexpected input", { left = pos; right = pos })
        fun (input: Input<'I>) (pos: Position) -> loop input pos [] es

    let SepBy (p: PackratParser<'I, 'A>) (sep: PackratParser<'I, 'B>) : PackratParser<'I, 'A list> =
        let rec loop (input: Input<'I>) (pos: Position) acc =
            match p input pos with
            | Success (value, nextPos) ->
                match sep input nextPos with
                | Success (_, nextPos2) -> loop input nextPos2 (value :: acc)
                | Failure _ -> Success ((List.rev (value :: acc)), nextPos)
            | Failure _ -> Success (List.rev acc, pos)
        fun input pos -> loop input pos []

    let SepBy1 (p: PackratParser<'I, 'A>) (sep: PackratParser<'I, 'B>) : PackratParser<'I, 'A list> =
        fun input pos ->
            match p input pos with
            | Success (value, nextPos) ->
                let rec loop input pos acc =
                    match sep input pos with
                    | Success (_, nextPos2) ->
                        match p input nextPos2 with
                        | Success (value, nextPos3) -> loop input nextPos3 (value :: acc)
                        | Failure _ -> Success (List.rev acc, pos)
                    | Failure _ -> Success (List.rev acc, pos)
                loop input nextPos [value]
            | Failure (reason, span) -> Failure (reason, span)

    let Once (p: PackratParser<'I, 'A>) (f: string * Span -> 'A) : PackratParser<'I, 'A> =
        fun input pos ->
            match p input pos with
            | Success (value, nextPos) -> Success (value, nextPos)
            | Failure (reason, _) ->
                let mutable current = pos
                while input.get current <> None do
                    current <- input.next current
                Success (f (reason, { left = pos; right = pos }), current)

    // 再帰的なパーサを定義するための遅延評価
    let Delay (f: unit -> PackratParser<'I,'A>) : PackratParser<'I,'A> =
        fun input -> (f()) input