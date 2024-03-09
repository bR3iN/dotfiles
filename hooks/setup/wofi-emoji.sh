#!/usr/bin/bash
SCRIPT_URL=https://raw.githubusercontent.com/Zeioth/wofi-emoji/master/wofi-emoji
TARGET_PATH=~/.local/bin/wofi-emoji

if [ ! -f "$TARGET_PATH" ]; then
    curl --create-dirs --output "$TARGET_PATH" "$SCRIPT_URL"
    chmod +x "$TARGET_PATH"
else
    echo "wofi-emoji is already installed"
fi
