# Lexer

## Source paths

- `src/Atla.Core/Syntax/Lexer.fs`
- `src/Atla.Core/Syntax/Data/Token.fs`
- `src/Atla.Core/Syntax/Data/SourceChar.fs`
- `src/Atla.Core/Syntax/Data/SourceString.fs`

## Current policy

The lexer converts source text into token data with spans. Source text details must not leak past AST construction.
