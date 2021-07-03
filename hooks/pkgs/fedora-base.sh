#!/usr/bin/bash

sudo dnf copr enable -y agriffis/neovim-nightly &>> /dev/null
sudo dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm

pkgs=(
    bat
    docker
    firefox
    fedora-workstation-repositories
    youtube-dl
    fish
    ImageMagick
    ffmpeg
    mpv
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
    torbrowser-launcher
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

sudo systemctl enable --now docker
sudo systemctl enable --now libvirtd
sudo systemctl enable --now tlp
