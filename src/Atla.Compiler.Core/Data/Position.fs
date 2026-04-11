namespace Atla.Compiler.Core.Data

type Position =
    { Line: int
      Column: int }

    static member Zero = { Line = 0; Column = 0 }

    member this.Advance(c: char) =
        if c = '\n' then { Line = this.Line + 1; Column = 0 }
        else { this with Column = this.Column + 1 }

    override this.ToString (): string =
        sprintf "%d:%d" this.Line this.Column