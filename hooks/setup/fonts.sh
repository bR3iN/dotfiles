#!/usr/bin/bash

set -o errexit
set -o pipefail
set -o nounset

FONT_DIR="/usr/local/share/fonts"

function install_fonts {
    local name="$1"
    local url="$2"
    # For idempotency; a match in $FONT_DIR indicates that the fonts are
    # already installed.
    local regex="$3"

    if ! ls "$FONT_DIR" 2>/dev/null | egrep -q "$regex"; then
        sudo mkdir -p "$FONT_DIR"
        echo "Downloading $name to $FONT_DIR"

        case "${url##*.}" in
            zip)
                local tmp="$FONT_DIR/tmp.zip"
                sudo wget "$url" -O "$tmp" &>> /dev/null
                echo "Extracting fonts"
                sudo unzip -d "$FONT_DIR" "$tmp" &>> /dev/null
                sudo rm "$tmp"
                ;;
            ttf)
                sudo wget -P "$FONT_DIR" "$url"
                ;;
            *)
                echo "URL $url points to file of unsupported type"
                exit 1
                ;;
        esac
    fi
}

install_fonts \
    "Fira Code Nerd Fonts" \
    'https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip' \
    'Fira.*Code.*Nerd Font'

install_fonts \
    "Fira Fonts" \
    'https://github.com/mozilla/Fira/archive/refs/tags/4.202.zip' \
    'Fira-.*'

install_fonts \
    "Symbols Nerd Font Complete" \
    'https://raw.githubusercontent.com/ryanoasis/nerd-fonts/master/patched-fonts/NerdFontsSymbolsOnly/complete/Symbols-1000-em%20Nerd%20Font%20Complete%20Mono.ttf' \
    'Symbols.*Nerd.*Font'
