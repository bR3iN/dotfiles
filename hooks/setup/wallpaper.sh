#!/usr/bin/bash
set -e
set -u

mkdir -p ~/Wallpaper/blurred

function add { ~/.local/bin/wm add --no-to-all "$@" || true; }

add --resize default 'https://raw.githubusercontent.com/pop-os/wallpapers/master/original/nasa-89125.jpg' 'pop-os'

local_files=(
    "/shared/Wallpaper/nord-fedora.png"
    "/shared/Wallpaper/nord.png"
    "/shared/Wallpaper/tumbleweed.png"
)

for file in "${local_files[@]}"; do
    if [ -f "$file" ]; then
        add "$file"
    fi
done

if [ ! -f ~/Wallpaper/current ]; then
    ~/.local/bin/wm set pop-os
fi

if [ ! -f ~/Wallpaper/default ]; then
    ~/.local/bin/wm set-default pop-os
fi
