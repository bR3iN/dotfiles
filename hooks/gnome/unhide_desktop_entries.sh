#!/usr/bin/bash

DATA_DIR="$HOME/.local/share/applications"
mkdir -p "$DATA_DIR"

for file in "/usr/share/applications/"gnome*panel.desktop; do
    base="$(basename "$file")"
    cat "$file" | sed '/NoDisplay/d' | sed '/OnlyShowIn/d' > "$DATA_DIR/$base"
done
