namespace Atla.Core.Syntax.Data

open Atla.Core.Data

type SourceString =
    { string: string
    ; span: Span }
    
    interface HasSpan with
        member this.span = this.span

    static member join (chars: SourceChar list): SourceString  =
        let str = System.String(Array.ofList (chars |> List.map (fun c -> c.char)))
        let span =
            match chars with
            | [] -> { left = { Line = 0; Column = 0 }; right = { Line = 0; Column = 0 } }
            | first :: rest ->
                let start = first.span.left
                let endPos =
                    chars
                    |> List.fold (fun (pos: Position) c -> pos.Advance(c.char)) start
                { left = start; right = endPos }
        { string = str; span = span }
