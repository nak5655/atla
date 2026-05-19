#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SOLUTION_PATH="$SCRIPT_DIR/src/Atla.slnx"
BIN_DIR="$HOME/.atla/bin"

if [ ! -f "$SOLUTION_PATH" ]; then
  echo "[ERROR] Solution file not found: $SOLUTION_PATH"
  exit 1
fi

mkdir -p "$BIN_DIR"

# Publish Atla.Console to ~/.atla/bin.
dotnet publish "$SCRIPT_DIR/src/Atla.Console/Atla.Console.fsproj" -c Release -o "$BIN_DIR"
echo "[OK] Atla.Console published to $BIN_DIR."

# Publish Atla.LanguageServer to ~/.atla/bin.
dotnet publish "$SCRIPT_DIR/src/Atla.LanguageServer/Atla.LanguageServer.fsproj" -c Release -o "$BIN_DIR"
echo "[OK] Atla.LanguageServer published to $BIN_DIR."

# Add ~/.atla/bin to PATH in shell profile if not already present.
add_to_path() {
  local profile="$1"
  if [ -f "$profile" ] && grep -q "$BIN_DIR" "$profile" 2>/dev/null; then
    return 0
  fi
  if [ -f "$profile" ]; then
    echo "" >> "$profile"
    echo "# Added by atla install" >> "$profile"
    echo "export PATH=\"\$PATH:$BIN_DIR\"" >> "$profile"
    return 0
  fi
  return 1
}

ADDED=false
for profile in "$HOME/.bashrc" "$HOME/.zshrc" "$HOME/.profile"; do
  if add_to_path "$profile"; then
    echo "[OK] Added $BIN_DIR to PATH in $profile."
    ADDED=true
    break
  fi
done

if [ "$ADDED" = false ]; then
  echo "[WARN] Could not find a shell profile to update. Add the following line manually:"
  echo "  export PATH=\"\$PATH:$BIN_DIR\""
fi

# Run `atla install` in the Std directory.
echo "[INFO] Running atla install in $SCRIPT_DIR/Std..."
cd "$SCRIPT_DIR/Std"
"$BIN_DIR/atla" install
echo "[OK] atla install completed."
