#!/usr/bin/bash

sudo dnf copr enable -y agriffis/neovim-nightly &>> /dev/null

pkgs=(
    awesome
    thunderbird
    fish
    xss-lock
    tmux
    npm
    rofi
    redshift
    picom
    xfce4-power-manager
    xfce4-screensaver-
    kitty
    firefox
    qutebrowser
    ImageMagick
    zathura
    meson
    ninja-build
    sassc
)

sudo dnf install "${pkgs[@]}"
