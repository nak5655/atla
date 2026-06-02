![Atla](logo.svg)

## Overview

Atla is a compiler, build toolchain, and VSCode extension for the Atla programming language, implemented in F#. Core components:

- **Atla.Console** — CLI frontend (`atla build`, `atla install`)
- **Atla.Build** — Project configuration (`atla.yaml`) and dependency resolution
- **Atla.Core** — Compiler pipeline (AST → Semantic Analysis → HIR → Closure Conversion → MIR → CIL)
- **Atla.LanguageServer** — Language Server Protocol backend for VSCode

## What is Atla?

Atla is a statically typed, multi-paradigm .NET language that supports Concatenative Programming and hierarchical algebraic data types (ADTs).

### Concatenative Programming

Values are placed before the function, and calls are chained with a trailing dot. Arguments are resolved at compile time (not stack-based at runtime).

**Atla:**
```atla
fn add (a: Int) (b: Int): Int = a + b

fn main: ()
    5 10 add. Console'WriteLine.
```

**Equivalent C#:**
```csharp
static int Add(int a, int b) => a + b;
static void Main() => Console.WriteLine(Add(5, 10));
```

### Hierarchical ADTs

Atla supports `union` types with nested variant hierarchies, enabling rich pattern matching over structured data.

**Atla:**
```atla
extendable union Color
    val alpha: Int
    object RichBlack: Color
        alpha = 255
    struct Rgb: Color
        val r: Int
        val g: Int
        val b: Int

impl Color
    fn red self: Int
        match self
        | Color'RichBlack -> 0
        | Color'Rgb { r, .. } -> r
```

**Equivalent C#:**
```csharp
abstract class Color { public abstract int Alpha { get; } }
sealed class RichBlack : Color { public override int Alpha => 255; }
sealed class Rgb : Color { public int R; public int G; public int B; public override int Alpha { get; set; } }
// pattern matching via switch expressions
```

## Installation

### Prerequisites

- .NET SDK (8.0 or later)

### Steps

**Windows:**
```bat
install.bat
```

**Linux / macOS:**
```bash
bash install.sh
```

Both scripts:
1. Publish `Atla.Console` and `Atla.LanguageServer` to `~/.atla/bin` (Windows: `%USERPROFILE%\.atla\bin`)
2. Add `~/.atla/bin` to `PATH`
3. Run `atla install` in the `Std/` directory to set up the standard library

### Post-install layout

```
~/.atla/
  bin/
    atla           # CLI (atla.exe on Windows)
    Atla.LanguageServer
    ...
```

## Examples

See the [`examples/`](examples/) directory for sample projects.

To build an example:

```bash
cd examples/hello
atla build
```

Output is placed in `out/` by default. For `package.type: exe`, run the resulting `.dll` with:

```bash
dotnet out/hello.dll
```

## For Contributors

Refer to [`developer_guidelines.md`](developer_guidelines.md) for compilation flow, phase invariants, IR invariants, and the `notes/` directory conventions.
