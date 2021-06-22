#!/usr/bin/bash

sudo dnf copr enable -y agriffis/neovim-nightly &>> /dev/null

pkgs=(
    bat
    docker
    firefox
    fish
    ImageMagick
    kitty
    neovim
    npm
    powertop
    qutebrowser
    redshift
    ristretto
    rofi
    setroubleshoot
    setxkbmap
    swaylock
    waybar
    thunderbird
    tlp
    tmux

    zathura
    zathura-pdf-poppler
    zathura-plugins-all
)


# Groups to install on top of group 'Fedora Server Edition'
groups=(
    "Core"
    "Standard"
    "Multimedia"
    "Printing Support"
    "Hardware Support"
    "Development Tools"
    "C Development Tools and Libraries"
    "Virtualization"
)

sudo dnf install -y "${pkgs[@]}"
sudo dnf group install -y "${groups[@]}"
