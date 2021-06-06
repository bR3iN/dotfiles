#!/usr/bin/bash
set -e

build_and_install() {
    TMP=$(mktemp -d)
    git clone https://github.com/Mange/rofi-emoji "$TMP"
    cd "$TMP"
    autoreconf -i
    mkdir build
    cd build/
    ../configure
    make
    sudo make install
}

[ ! -d /usr/local/share/rofi-emoji ] && build_and_install
exit 0
