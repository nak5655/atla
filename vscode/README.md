# Atla — VS Code Extension

VS Code language support extension for the **Atla** programming language.  
Provides real-time diagnostics, completions, hover information, and other IntelliSense features via the Language Server Protocol (LSP).

## Features

- Real-time error diagnostics (via LSP)
- Code completions, hover information, and signature help
- Build / Clean / Run tasks (VS Code task runner integration)
- Problem matcher (displays compiler errors in the Problems panel)

## Requirements

| Requirement | Version |
|-------------|---------|
| VS Code | ≥ 1.90.0 |
| Node.js | ≥ 22 |
| `atla-lsp.exe` | Must be installed separately |
| `atla.exe` | Must be installed separately |

`atla-lsp.exe` and `atla.exe` are expected to be located at `~/.atla/` by default.  
See the [Atla official website](https://atla-lang.org/) for installation instructions.

## Installation

Install from the VS Code Marketplace or directly from a `.vsix` file.

```bash
# Install from a .vsix file
code --install-extension atla-0.0.1.vsix
```

## Configuration

Extension settings can be changed in VS Code's settings (`settings.json`).

| Setting key | Default | Description |
|-------------|---------|-------------|
| `atla.lsp-server.path` | `~/.atla/atla-lsp.exe` | Path to the Atla LSP server executable |
| `atla.build-system.path` | `~/.atla/atla.exe` | Path to the Atla build system executable |

Example:

```jsonc
// .vscode/settings.json
{
  "atla.lsp-server.path": "C:/tools/atla/atla-lsp.exe",
  "atla.build-system.path": "C:/tools/atla/atla.exe"
}
```

## Build Instructions (for developers)

### 1. Clone the repository and install dependencies

```bash
git clone https://github.com/nak5655/vscode-atla.git
cd vscode-atla
npm install
```

### 2. Available commands

| Command | Description |
|---------|-------------|
| `npm run build` | Bundle the extension (output: `dist/extension.js`) |
| `npm run dev` | Watch mode — automatically rebuilds on file changes |
| `npm run typecheck` | Type-check without emitting files |
| `npm run lint` | Run ESLint on `src/` |
| `npm run format` | Format code with Prettier |
| `npm run check` | Run `lint` + `typecheck` + `build` together (used before publishing) |

### 3. Standard build

```bash
npm run build
```

Generates `dist/extension.js`.

### 4. Development (watch mode)

```bash
npm run dev
```

Automatically rebuilds whenever a file is saved. Reload the VS Code extension host to apply changes.

### 5. Pre-publish check

```bash
npm run check
```

Runs ESLint, type checking, and the build together. Always run this before opening a PR.

### 6. Packaging (.vsix)

```bash
npx vsce package
```

Generates `atla-<version>.vsix` in the current directory.

## License

[Apache License 2.0](LICENSE)
