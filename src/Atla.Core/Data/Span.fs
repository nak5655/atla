namespace Atla.Core.Data

type Span =
    { left: Position; right: Position }

    static member Empty = { left = Position.Zero; right = Position.Zero }

    override this.ToString (): string =
        sprintf "[%s, %s)" (this.left.ToString()) (this.right.ToString())

type HasSpan =
    abstract span: Span
