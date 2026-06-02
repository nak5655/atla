![Atla](logo.svg)

## Overview

Atla is a compiler and build toolchain, implemented in F#. Core components:

- **Atla.Console** — CLI frontend (`atla build`, `atla install`)
- **Atla.Build** — Project configuration (`atla.yaml`) and dependency resolution
- **Atla.Core** — Compiler pipeline
- **Atla.LanguageServer** — Language Server Protocol backend for VSCode

This repository contains a VSCode extension for the Atla programming language.

## What is Atla?

Atla is a statically typed, multi-paradigm .NET language that supports Concatenative Programming and hierarchical algebraic data types (ADTs).

### Concatenative Programming

You simply chain a function after the argument. This matches a stack‑based flow, making it intuitive and more concise.

**Atla:**
```atla
fn main: ()
    Console'ReadLine. Int32'Parse. 0 Math'Max. Console'WriteLine.
```

**Equivalent C#:**
```csharp
static void Main()
{
    Console.WriteLine(Math.Max(0, Int32.Parse(Console.ReadLine())));
}
```

### Hierarchical ADTs

Atla supports `union` types with nested variant hierarchies, enabling rich pattern matching over structured data.

**Atla:**
```atla
union Color
    val alpha: Int

    object Gold: Color
        alpha = 255

    struct Rgb: Color
        val r: Int
        val g: Int
        val b: Int

    union Hsx: Color
        val h: Int
        val s: Int

        struct Hsv: Hsx
            val v: Int

        struct Hsl: Hsx
            val l: Int

impl Color
    fn red self: Int
        match self
        | Color'Gold -> 255
        | Color'Rgb { r, .. } -> r
        | Color'Hsx'Hsv { h, s, v, .. } -> (h * s * v) / 10000
        | Color'Hsx'Hsl { h, s, l, .. } -> (h * s * l) / 10000
```

**Equivalent C#:**
```csharp
public abstract record Color(int Alpha)
{
    public sealed record Gold() : Color(255)
    {
        public static readonly Gold Instance = new();
    }

    public readonly record struct Rgb(int R, int G, int B, int Alpha) : Color(Alpha);

    public abstract record Hsx(int H, int S, int Alpha) : Color(Alpha)
    {
        public readonly record struct Hsv(int H, int S, int V, int Alpha) : Hsx(H, S, Alpha);

        public readonly record struct Hsl(int H, int S, int L, int Alpha) : Hsx(H, S, Alpha);
    }

    public int Red()
    {
        return this switch
        {
            Gold => 255,
            Rgb rgb => rgb.R,
            Hsx.Hsv hsv => (hsv.H * hsv.S * hsv.V) / 10000,
            Hsx.Hsl hsl => (hsl.H * hsl.S * hsl.L) / 10000,
            _ => throw new NotImplementedException()
        };
    }
}
```

C#’s `abstract record` is an OOP-style abstract class, not an algebraic data type (ADT). Because of that, the following properties are lost:
- The meaning that Color is a set of variants.
- The guarantee that the types under Color are closed (a sealed union).

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
    atla-lsp       # LSP Server (atla-lsp.exe on Windows)
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

Refer to [`GUIDELINES.md`](GUIDELINES.md) for compilation flow, phase invariants, IR invariants, and the `notes/` directory conventions.
