#!/bin/bash

PAQ_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/pack/paqs/opt/paq-nvim" 

if [ ! -e "$PAQ_PATH" ]; then
    git clone https://github.com/savq/paq-nvim.git "$PAQ_PATH"
fi
