namespace Atla.Parser

type SourceString = 
    { Str: string; Span: Span }
    interface HasSpan with
        member this.GetSpan() = this.Span
