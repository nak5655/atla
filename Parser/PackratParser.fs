namespace Atla.Parser

type PackratParser<'I> =
    member satisfy name pred: Parser