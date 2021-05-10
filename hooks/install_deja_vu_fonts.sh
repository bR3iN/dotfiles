#!/usr/bin/bash

FONT_PATH="$HOME/.local/share/fonts"
TMP="$FONT_PATH/tmp.zip"

FONT_NAME="DejaVu Fonts"
URL='https://github.com/dejavu-fonts/dejavu-fonts/releases/download/version_2_37/dejavu-fonts-ttf-2.37.zip'

if ! ls "$FONT_PATH" | egrep -i -q 'Deja.*Vu.*Sans'; then
    mkdir -p "$FONT_PATH"
    echo "Downloading $FONT_NAME to $HOME/.local/share/fonts..."
    wget "$URL" -O "$FONT_PATH/tmp.zip" &>> /dev/null
    echo "Extracting fonts..."
    unzip -d "$FONT_PATH/tmp" "$TMP" &>> /dev/null
    cp "$FONT_PATH/tmp/dejavu-fonts-ttf-2.37/ttf/"* "$FONT_PATH/"
    rm "$TMP"
    rm -r "$FONT_PATH/tmp"
fi
