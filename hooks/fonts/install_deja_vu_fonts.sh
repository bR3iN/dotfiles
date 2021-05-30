#!/usr/bin/bash

FONT_NAME="DejaVu Fonts"
URL='https://github.com/dejavu-fonts/dejavu-fonts/releases/download/version_2_37/dejavu-fonts-ttf-2.37.zip'
REGEX='dejavu-fonts-ttf.*'

ROOT_FONT_DIR="/usr/local/share/fonts"
USER_FONT_DIR="$HOME/.local/share/fonts"

if [[ "${1-}" =~ (-g|--global) ]]; then
    FONT_DIR="$ROOT_FONT_DIR"
    ROOT=true
else
    FONT_DIR="$USER_FONT_DIR"
fi

function is_installed {
    if ls "$ROOT_FONT_DIR" 2>/dev/null | egrep -i -q "$REGEX"; then
        return 0
    elif [ "$ROOT" != true ] && ls "$USER_FONT_DIR" 2>/dev/null | egrep -i -q "$REGEX"; then
        return 0
    else
        return 1
    fi
}

if ! is_installed; then
    ${ROOT:+sudo} mkdir -p "$FONT_DIR"
    TMP="$FONT_DIR/tmp.zip"
    echo "Downloading $FONT_NAME to $FONT_DIR..."
    ${ROOT:+sudo} wget "$URL" -O "$TMP" &>> /dev/null
    echo "Extracting fonts..."
    ${ROOT:+sudo} unzip -d "$FONT_DIR" "$TMP" &>> /dev/null
    ${ROOT:+sudo} rm "$TMP"
fi
