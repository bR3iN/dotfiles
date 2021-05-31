#!/usr/bin/bash

sudo dnf copr enable -y agriffis/neovim-nightly &>> /dev/null

#gvfs
pkgs=(
    awesome
    blueberry
    bat
    firefox
    fish
    google-noto-emoji-color-fonts
    ImageMagick
    kitty
    lightdm-gtk
    NetworkManager-tui
    npm
    parole
    pavucontrol
    picom
    pragha
    qutebrowser
    redshift
    rofi
    Thunar
    thunar-volman
    thunderbird
    tmux
    tumbler
    xfce-polkit
    xfce4-power-manager
    xfce4-settings
    xfce4-taskmanager
    xfconf
    xss-lock
    zathura
    meson
    ninja-build
    sassc
    glib2-devel
)

groups=(
    "Core"
    "Standard"
    "Multimedia"
    "Printing Support"
    "base-x"
    "Hardware Support"
    "Development Tools"
    "C Development Tools and Libraries"
)

sudo dnf install "${pkgs[@]}"
sudo dnf group install "${groups[@]}"
