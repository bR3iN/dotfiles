#!/usr/bin/bash
set -e
set -u

mkdir -p ~/Wallpaper/blurred

function add { ~/.local/bin/wm add --no-to-all "$@" || true; }

add --resize default 'https://raw.githubusercontent.com/pop-os/wallpapers/master/original/nasa-89125.jpg' 'Pop-OS'

add "/shared/Wallpaper/nord.png" "Nord"
add "/shared/Wallpaper/tumbleweed.png" "OpenSUSE(dark)"

if [ ! -f ~/Wallpaper/current ]; then
    ~/.local/bin/wm set Pop-OS
fi

if [ ! -f ~/Wallpaper/default ]; then
    ~/.local/bin/wm set-default Pop-OS
fi
