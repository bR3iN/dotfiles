#!/usr/bin/bash

sudo dnf copr enable -y agriffis/neovim-nightly &>> /dev/null

pkgs=(
    bat
    docker
    firefox
    youtube-dl
    fish
    ImageMagick
    kitty
    neovim
    npm
    powertop
    qutebrowser
    redshift
    ristretto
    #rofi
    setroubleshoot
    setxkbmap
    thunderbird
    tlp
    tmux

    # Order matters
    zathura
    zathura-pdf-poppler
    zathura-plugins-all

    # For building flavours
    cargo
)

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
