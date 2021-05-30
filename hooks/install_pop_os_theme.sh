#!/usr/bin/bash
set -e

TMP=$(mktemp -d)

install_gtk_theme() {
    git clone https://github.com/pop-os/gtk-theme "$TMP/pop-gtk"
    cd "$TMP/pop-gtk"
    meson build && cd build
    ninja
    ninja install
}

install_icon_theme() {
    git clone https://github.com/pop-os/icon-theme "$TMP/pop-icons"
    cd "$TMP/pop-icons"
    meson build
    sudo ninja -C "build" install
}

[ ! -d /usr/share/themes/Pop ] && install_gtk_theme || true
[ ! -d /usr/share/icons/Pop ] && install_icon_theme || true
