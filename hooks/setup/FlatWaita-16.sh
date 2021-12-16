#!/usr/bin/bash

URL="https://github.com/bR3iN/FlatWaita-16"
DIR="$HOME/.themes/FlatWaita-16"

if [ ! -d "$DIR" ]; then
    git clone "$URL" "$DIR"
    (cd "$DIR" && make install)

    if [ -x /usr/bin/gsettings ]; then
        gsettings set org.gnome.desktop.interface gtk-theme 'FlatColor'
    fi
fi
