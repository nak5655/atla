namespace Atla.Compiler.Lowering.Data

open System.Collections.Generic
open Atla.Compiler.Semantics.Data

type Frame() =
    let mutable _args: Dictionary<SymbolId, Mir.Reg> = Dictionary()
    let mutable _locs: Dictionary<SymbolId, Mir.Reg> = Dictionary()
    member this.args = _args
    member this.locs = _locs

    member this.addArg(sym: SymbolId): Mir.Reg =
        let reg = Mir.Reg.Arg(_args.Count - 1)
        _args.Add(sym, reg)
        reg
        
    member this.addLoc(sym: SymbolId): Mir.Reg =
        let reg = Mir.Reg.Loc(_locs.Count - 1)
        _locs.Add(sym, reg)
        reg
       
    member this.get(sym: SymbolId): Mir.Reg option =
        if _args.ContainsKey(sym) then Some _args.[sym]
        elif _locs.ContainsKey(sym) then Some _locs.[sym]
        else None
