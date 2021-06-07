#!/usr/bin/bash

sudo dnf copr enable -y agriffis/neovim-nightly &>> /dev/null

#gvfs
pkgs=(
    awesome
    bat
    blueberry
    firefox
    fish
    google-noto-emoji-color-fonts
    ImageMagick
    kitty
    lightdm-gtk
    neovim
    NetworkManager-tui
    npm
    parole
    pavucontrol
    picom
    pragha
    qutebrowser
    redshift
    rofi
    setxkbmap
    Thunar
    thunar-volman
    thunderbird
    tmux
    tumbler
    volumeicon
    xfce-polkit
    xfce4-power-manager
    xfce4-settings
    xfce4-taskmanager
    xfconf
    xss-lock
    zathura

    # For building the pop-os gtk themes and rofi-emoji
    meson
    ninja-build
    sassc
    glib2-devel
    cairo-devel
    rofi-devel
)

# Groups to install on top of group 'Fedora Server Edition'
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

sudo dnf install -y "${pkgs[@]}"
sudo dnf group install -y "${groups[@]}"
