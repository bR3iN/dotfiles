#!/usr/bin/bash
set -e
set -u

# Used for history file
mkdir -p "${XDG_DATA_HOME:-$HOME/.local/share}/zsh"

PLUGIN_DIR="${ZDOTDIR:-$HOME/.config/zsh}/plugins"

mkdir -p "$PLUGIN_DIR"
pushd "$PLUGIN_DIR"

urls=(
    https://github.com/zsh-users/zsh-syntax-highlighting
    https://github.com/zsh-users/zsh-autosuggestions
    # https://github.com/jeffreytse/zsh-vi-mode
    https://github.com/marlonrichert/zsh-autocomplete
)

for url in "${urls[@]}"; do
    git clone "$url" || true
done

sudo usermod --shell=/usr/bin/zsh "$(whoami)"
