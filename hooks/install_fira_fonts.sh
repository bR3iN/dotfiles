#!/usr/bin/bash

FONT_PATH="$HOME/.local/share/fonts"
TMP="$FONT_PATH/tmp.zip"

FONT_NAME="Fira Fonts"
URL='https://github.com/mozilla/Fira/archive/refs/tags/4.202.zip'

if ! ls "$FONT_PATH" | egrep -i -q 'Fira.*Sans.*Regular'; then
    mkdir -p "$FONT_PATH"
    echo "Downloading $FONT_NAME to $HOME/.local/share/fonts..."
    wget "$URL" -O "$FONT_PATH/tmp.zip" &>> /dev/null
    echo "Extracting fonts..."
    unzip -d "$FONT_PATH/tmp" "$TMP" &>> /dev/null
    cp "$FONT_PATH/tmp/Fira-4.202/ttf/"* "$FONT_PATH/"
    rm "$TMP"
    rm -r "$FONT_PATH/tmp"
fi
