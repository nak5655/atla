# Parser

## Source paths

- `src/Atla.Core/Syntax/Parser.fs`
- `src/Atla.Core/Syntax/PackratParser.fs`
- `src/Atla.Core/Syntax/Data/ParseResult.fs`
- `src/Atla.Core/Syntax/Data/Input.fs`

## Current policy

The parser produces AST only. It must not attach type, scope, or lowering information to AST nodes.
