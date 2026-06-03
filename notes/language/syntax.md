# Syntax

## Source paths

- `src/Atla.Core/Syntax/Lexer.fs`
- `src/Atla.Core/Syntax/Parser.fs`
- `src/Atla.Core/Syntax/Data/Token.fs`
- `src/Atla.Core/Syntax/Data/Ast.fs`

## Current policy

Syntax notes are user-facing. Parser combinator implementation details belong in `notes/compiler/syntax/`.

Keep this file focused on stable source forms and indentation/offside behavior. When a syntax rule is only an implementation detail, document it in the compiler syntax notes instead.
