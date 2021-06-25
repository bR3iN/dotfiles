#!/usr/bin/bash
set -e
set -u

LOCAL_WALLPAPER_DIR="$HOME/Wallpaper"
SYSTEM_WALLPAPER_DIR="/usr/share/backgrounds/mywallpaper"
TARGET_SIZE="1920x1080"

download_wallpaper() {
    local name="$1"
    local url="$2"
    local directory="$3"

    echo "Downloading wallpaper ${name} to ${directory}"
    wget -O "$directory/${name}.${url##*.}" "$url" &>> /dev/null
}

resize_and_blur() {
    local path="$1"
    local path_blurred="$2"

    echo "Resizing wallaper"
    convert -resize "${TARGET_SIZE%%x*}" "$path" "$path"

    echo "Blurring wallpaper for lock screen"
    convert -blur 0x30 "$path" "$path_blurred"
}

install_wallpaper() {
    if [ "$1" = "--root" ]; then
        local as_root="true"
        shift
    fi

    local name="$1"
    local url="$2"
    local extension="${url##*.}"

    # Check if wallpaper is already installed
    if [ "${as_root-}" = true ]; then
        if [ -f "$SYSTEM_WALLPAPER_DIR/$name.$extension" ]; then
            echo "Wallpaper already installed. Skipping..."
            return
        fi
    else
        if [ -f "$LOCAL_WALLPAPER_DIR/$name.$extension" ]; then
            echo "Wallpaper already installed. Skipping..."
            return
        fi
    fi

    if [ ! "${as_root-}" = true ]; then
        local directory="$LOCAL_WALLPAPER_DIR"
    else
        local tmp_dir="$(mktemp -d)"
        local directory=$tmp_dir
    fi

    download_wallpaper "$name" "$url" "$directory"

    local path="$directory/$name.$extension"
    local path_blurred="$directory/${name}_blurred.$extension"
    resize_and_blur "$path" "$path_blurred"

    if [  "${as_root-}" = "true" ]; then
        echo "Installing wallpaper..."
        sudo mkdir -p "$SYSTEM_WALLPAPER_DIR"
        sudo install -m0644 -D --target-directory="$SYSTEM_WALLPAPER_DIR" "$path"
        sudo install -m0644 -D --target-directory="$SYSTEM_WALLPAPER_DIR" "$path_blurred"
        rm -r "${tmp_dir:?}"
    fi
}


#echo "Download wallpaper..."

#echo "Convert wallpaper to PNG, crop wallpaper and blur wallpaper for login and lock screen..."
#convert -resize "${TARGET_SIZE%%x*}" "$TEMP_DIR/original.$EXTENSION" "$TEMP_DIR/mywallpaper.png"
#convert -crop "${TARGET_SIZE}+${X_OFFSET}+${Y_OFFSET}" "$TEMP_DIR/mywallpaper.png" "$TEMP_DIR/mywallpaper.png"
#convert -blur 0x30 "$TEMP_DIR/mywallpaper.png" "$TEMP_DIR/mywallpaper-blurred.png"

#echo "Installing wallpaper..."
#sudo install -m0644 -D --target-directory="$WALLPAPER_DIR" "$TEMP_DIR/mywallpaper.png"
#sudo install -m0644 -D --target-directory="$WALLPAPER_DIR" "$TEMP_DIR/mywallpaper-blurred.png"

#rm -r "${TEMP_DIR:?}"

install_wallpaper "pop-os" "https://raw.githubusercontent.com/pop-os/wallpapers/master/original/nasa-89125.jpg"
install_wallpaper "nord1" "https://www.itl.cat/pngfile/big/293-2936366_3840x2160-knstlerisch-minimalist-wallpaper-mountain.jpg"
install_wallpaper "nord" "https://i.redd.it/jkxvgyorlk051.png"
