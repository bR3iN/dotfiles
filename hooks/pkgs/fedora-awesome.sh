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
    gnome-shell-extension-user-theme
    gnome-extensions-app
    ImageMagick
    zathura
    meson
    ninja-build
    sassc
    glib2-devel
)

sudo dnf install "${pkgs[@]}"
