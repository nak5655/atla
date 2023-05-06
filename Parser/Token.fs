namespace Atla.Parser

type TokenKind =
| Comment
| Keyword
| Delim
| InfixOp1 // precedence (greater means higher)
| InfixOp2
| InfixOp3
| InfixOp4
| InfixOp5
| InfixOp6
| UnknownId
| TypeId
| FieldId
| FuncId
| LocalVarId
| Int
| Float
| Double
| String

type Token = { Kind: TokenKind; Text: string }