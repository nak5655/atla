namespace Atla.Compiler.Types

type Symbol(name: string, typ: System.Type) =
    member this.name = name
    member this.typ = typ
    override this.ToString() = sprintf "Symbol(%s: %s)" name (typ.FullName)