namespace Atla.Parser

type Span = { lo: Position; hi: Position }

type HasSpan =
    abstract GetSpan: unit -> Span