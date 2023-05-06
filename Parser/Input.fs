namespace Atla.Parser

[<AbstractClass>]
type Input<'I>() =
    abstract member WhereIs: unit -> System.Object
    abstract member Get: unit -> 'I option
    abstract member Next: unit -> Input<'I>