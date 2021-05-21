#!/usr/bin/bash

FONT_PATH="$HOME/.local/share/fonts"
TMP="$FONT_PATH/tmp.zip"

FONT_NAME="Fira Fonts"
URL='https://github.com/mozilla/Fira/archive/refs/tags/4.202.zip'

if ! ls "$FONT_PATH" | egrep -q 'Fira-.*'; then
    mkdir -p "$FONT_PATH"
    echo "Downloading $FONT_NAME to $HOME/.local/share/fonts..."
    wget "$URL" -O "$TMP" &>> /dev/null
    echo "Extracting fonts..."
    unzip -d "$FONT_PATH" "$TMP" &>> /dev/null
    rm "$TMP"
fi
