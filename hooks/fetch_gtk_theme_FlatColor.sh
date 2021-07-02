#!/usr/bin/bash

URL="https://github.com/jasperro/FlatColor"

if [ ! -d ~/.themes/FlatColor ]; then
    git clone "$URL" ~/.themes/FlatColor
fi

if [ ! -d /usr/share/themes/FlatColor ]; then
    sudo cp -r ~/.themes/FlatColor /usr/share/themes/
    echo '@import "flavours.css";' | sudo tee -a /usr/share/themes/FlatColor/gtk-3.0/gtk.css  >/dev/null
    echo '@import "flavours.css";' | sudo tee -a /usr/share/themes/FlatColor/gtk-3.20/gtk.css >/dev/null
fi
