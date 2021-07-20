#!/usr/bin/bash
set -e
set -u

WALLPAPER_DIR="$HOME/Wallpaper"

main() {
    if [ -n "$*" ]; then
        install_wallpaper "$@"
    else
        default_selection
    fi
}

default_selection() {
    # Wallpapers to be installed on all distros
    install_wallpaper --both --resize "pop-os" "https://raw.githubusercontent.com/pop-os/wallpapers/master/original/nasa-89125.jpg"
    install_wallpaper "opensuse"      "/shared/Wallpaper/tumbleweed.png"
    install_wallpaper "opensuse_wide" "/shared/Wallpaper/tumbleweed-wide.png"

    # Distro specific wallpaper
    case "$(cat /etc/os-release | grep "^NAME")" in
        *Fedora)
            install_wallpaper "nord"      "/shared/Wallpaper/nord-fedora.png"
            install_wallpaper "nord_wide" "/shared/Wallpaper/nord-fedora-wide.png"
            ;;
        *Tumbleweed*)
            install_wallpaper "nord"      "/shared/Wallpaper/nord-tumbleweed.png"
            install_wallpaper "nord_wide" "/shared/Wallpaper/nord-tumbleweed-wide.png"
            ;;
        *)
            install_wallpaper "nord"      "/shared/Wallpaper/nord.png"
            install_wallpaper "nord_wide" "/shared/Wallpaper/nord-wide.png"
    esac
}

install_wallpaper() {
    while [[ "$1" =~ ^- ]]; do
        case "$1" in
            -f|--force)
                local FORCE=true
                ;;
            -b|--both)
                local BOTH=true
                ;;
            -r|--resize)
                local RESIZE=true
        esac
        shift
    done

    local name="$1"
    local url="$2"
    local extension="${url##*.}"
    local file="${name}.$extension"

    if [ -f "$WALLPAPER_DIR/$file" ] && [ -z "${FORCE-}" ]; then
        echo "Wallpaper with this name already installed. Skipping..."
        return
    fi

    local TMP_DIR=$(mktemp -d)

    if [[ "$url" =~ ^/ ]]; then
        get_wallpaper "$url" || {
            echo "WARNING: Path $url is not valid. Skipping..."
            return
        }
    else
        download_wallpaper "$url" || {
            echo "WARNING: Url $url is not valid. Skipping..."
            return
        }
    fi

    process "$name" "$extension"
    if [ "${BOTH-}" = true ]; then
        process "${name}_wide" "$extension"
    fi

    rm -r "${TMP_DIR?}"
}

process() {
    local name="$1"
    local extension="$2"
    local file="${name}.$extension"

    if [ "${RESIZE-}" = true ]; then
        if [[ "$name" =~ _wide$ ]]; then
            resize 3440x1440 "$TMP_DIR/tmp.$extension" "$WALLPAPER_DIR/$file"
        else
            resize 1920x1080 "$TMP_DIR/tmp.$extension" "$WALLPAPER_DIR/$file"
        fi
    else
        cp "$TMP_DIR/tmp.$extension" "$WALLPAPER_DIR/$file"
    fi

    blur "$WALLPAPER_DIR/$file"
}

resize() {
    local target_size="$1"
    local file="$2"
    local target_file="$3"

    echo "Resizing wallpaper to $target_size"
    convert -resize "${target_size%%x*}" -crop "${target_size}+0+0" -gravity center "$file" "$target_file"
}

blur() {
    local path="$1"
    echo "Blurring wallpaper for lock screen"
    convert -blur 0x30 "$path" "${path%.*}_blurred.${file##*.}"
}

download_wallpaper() {
    local url="$1"
    echo "Downloading wallpaper from $url"
    wget -O "$TMP_DIR/tmp.${url##*.}" "$url" &>> /dev/null
}

get_wallpaper() {
    local path="$1"
    [ -f "$path" ] || return 1

    echo "Fetching wallpaper from ${path}"
    cp "$path" "$TMP_DIR/tmp.${path##*.}"
}


main "$@"
