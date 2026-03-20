namespace Atla.Compiler.Mir

type Symbol(name: string, typ: System.Type) =
    member this.name = name
    member this.typ = typ
    override this.ToString() = sprintf "Symbol(%s: %s)" name (typ.FullName)
    override this.Equals (obj: obj): bool =
        match obj with
        | :? Symbol as other -> this.name = other.name && this.typ = other.typ
        | _ -> false
    override this.GetHashCode(): int =
        hash (this.name, this.typ)