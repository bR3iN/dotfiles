#!/usr/bin/bash

set -o errexit
set -o pipefail
set -o nounset

FONT_DIR="/usr/local/share/fonts"

for cmd in wget unzip sudo; do
    if ! which "$cmd" &>/dev/null; then
        echo "ERROR: '$cmd' not found"
        exit 1
    fi
done

function install_fonts {
    local name="$1"
    local url="$2"
    local regex="$3"

    # Rest of arguments are file patterns matched in `unzip` below.
    shift; shift; shift

    if ls "$FONT_DIR" 2>/dev/null | grep -q "$regex"; then
        echo "'$name' is already installed"
        return
    fi

    sudo mkdir -p "$FONT_DIR"
    echo "Downloading '$name'"

    local tmp="$FONT_DIR/$name.zip"
    if [ ! -f "$tmp" ]; then
        sudo wget "$url" -O "$tmp" &>> /dev/null
    fi

    echo "Extracting fonts"
    sudo unzip -d "$FONT_DIR" "$tmp" "$@" >/dev/null
    sudo rm "$tmp"
}

install_fonts \
    'Noto Fonts' \
    'https://github.com/ryanoasis/nerd-fonts/releases/download/v2.3.3/Noto.zip' \
    'Noto Sans.*Nerd Font Complete.ttf' \
    'Noto Sans * Nerd Font Complete.ttf' \
    'Noto Serif * Nerd Font Complete.ttf'

install_fonts \
    'Fira Code Nerd Fonts' \
    'https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip' \
    'Fira Code.*Nerd Font Complete.ttf' \
    'Fira Code * Nerd Font Complete.ttf'
