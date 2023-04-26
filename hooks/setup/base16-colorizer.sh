#!/usr/bin/bash
if ! which base16-colorizer &> /dev/null; then
    mkdir -p ~/.local/bin
    wget --output-document ~/.local/bin/base16-colorizer \
        https://github.com/bR3iN/base16-colorizer/releases/download/v0.1.0/base16-colorizer-v0.1.0-x86_64-linux
    chmod +x ~/.local/bin/base16-colorizer
fi
