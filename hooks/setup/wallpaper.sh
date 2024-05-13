#!/usr/bin/bash
set -e
set -u

WALLPAPER_DIR="$HOME/Wallpaper"
mkdir -p "$WALLPAPER_DIR"

download-to() {
    local out_path="$1"
    local url="$2"
    echo "downloading $1"
    curl -o "$out_path" "$url"
}

copy-to() {
    if [ -f "$1" ]; then
        echo "copying $1"
        cp "$1" "$2"
    else
        echo "WARNING: $1 does not exist or is not a file"
    fi
}

resize() {
    local path_in="$1"
    local path_out="$2"
    local width="$3"
    local height="$4"
    echo "resizing $path_in"
    convert -resize "$width" -crop "${width}x${height}+0+0" -gravity center "$path_in" "$path_out"
}

download-to "$WALLPAPER_DIR/Pop-OS.jpg" 'https://raw.githubusercontent.com/pop-os/wallpapers/master/original/nasa-89125.jpg'
# Just change the out extension to also convert format
resize "$WALLPAPER_DIR/Pop-OS.jpg" "$WALLPAPER_DIR/Pop-OS.jpg" 3840 2160

download-to "$WALLPAPER_DIR/Tokyonight.jpg" 'https://getwallpapers.com/wallpaper/full/c/3/4/49335.jpg'

download-to "$WALLPAPER_DIR/Catpuccin.png" 'https://preview.redd.it/catpuccin-wallpapers-6094x2344-3440x1440-v0-ycpfbe13hxu91.png?width=6094&format=png&auto=webp&s=5fa2a8980b1124ae05b9fa4e4b7c53a37f2ac505'
resize "$WALLPAPER_DIR/Catpuccin.png" "$WALLPAPER_DIR/Catpuccin.png" 3840 2160

download-to "$WALLPAPER_DIR/Gruvbox.webp" 'https://preview.redd.it/gruvbox-abstract-shapes-7680x4320-v0-uhig907t7dca1.png?auto=webp&s=c7ebd03163080183a7e498e2c3f3f3bdb3aaebb3'
resize "$WALLPAPER_DIR/Gruvbox.webp" "$WALLPAPER_DIR/Gruvbox.png" 3840 2160
rm "$WALLPAPER_DIR/Gruvbox.webp"

copy-to "/shared/Wallpaper/nord.png" "$WALLPAPER_DIR/Nord.png"
copy-to "/shared/Wallpaper/tumbleweed.png" "$WALLPAPER_DIR/OpenSUSE(dark).png"
