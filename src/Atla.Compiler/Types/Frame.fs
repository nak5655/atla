namespace Atla.Compiler.Types

type FramePosition =
    | Arg of index: int
    | Loc of index: int

type Frame() =
    let mutable _args: List<Symbol> = []
    let mutable _locs: List<Symbol> = []

    member this.args = _args
    member this.locs = _locs

    member this.declareArg(symbol: Symbol) =
        _args <- _args @ [symbol]

    member this.declareLoc(symbol: Symbol) =
        _locs <- _locs @ [symbol]

    member this.declareTemp(typ: System.Type): Symbol =
        let temp = sprintf "$temp%d" (List.length _locs)
        let sym = Symbol(temp, typ)
        _locs <- _locs @ [sym]
        sym

    member this.resolve(symbol: Symbol): FramePosition option =
        match List.tryFindIndex ((=) symbol) _args with
        | Some index -> Some (Arg index)
        | None ->
            match List.tryFindIndex ((=) symbol) _locs with
            | Some index -> Some (Loc index)
            | None -> None