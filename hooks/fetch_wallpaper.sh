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

get_wallpaper() {
    local name="$1"
    local path="$2"
    local directory="$3"
    echo "Copying wallpaper ${name} to ${directory}"
    cp "$path" "$directory/${name}.${path##*.}"
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
    while [[ "$1" =~ ^-- ]]; do
        case "$1" in
            --root)
                local as_root="true"
                ;;
            --local)
                local from_path="true"
                ;;
        esac
        shift
    done

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
        local tmp_dir
        tmp_dir="$(mktemp -d)"
        local directory="$tmp_dir"
    fi

    if [ "$from_path" = true ]; then
        get_wallpaper "$name" "$url" "$directory"
    else
        download_wallpaper "$name" "$url" "$directory"
    fi

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

# On all systems
install_wallpaper "pop-os" "https://raw.githubusercontent.com/pop-os/wallpapers/master/original/nasa-89125.jpg"

case "$(cat /etc/os-release | grep "^NAME")" in
	*Fedora)
		install_wallpaper         "nord"     "https://i.redd.it/jkxvgyorlk051.png"
		install_wallpaper --local "fedora34" "/usr/share/backgrounds/default.png"
		install_wallpaper --local "fedora"   "/usr/share/backgrounds/fedora-workstation/paisaje.jpg"
		;;
	*Tumbleweed*)
		install_wallpaper --local "opensuse" "/usr/share/wallpapers/default-1920x1080.jpg"
esac
