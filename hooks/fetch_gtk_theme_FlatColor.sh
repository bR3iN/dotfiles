#!/usr/bin/bash

URL="https://github.com/jasperro/FlatColor"
if [ ! -d ~/.themes/FlatColor ]; then
    git clone "$URL" ~/.themes/FlatColor
fi
