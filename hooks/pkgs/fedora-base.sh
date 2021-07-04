#!/usr/bin/bash

sudo dnf copr enable -y agriffis/neovim-nightly &>> /dev/null
sudo dnf install -y https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm

pkgs=(
    #rofi
    bat
    docker
    fedora-workstation-repositories
    ffmpeg
    firefox
    fish
    gparted
    ImageMagick
    kitty
    mpv
    mpv-mpris
    neovim
    npm
    plymouth-theme-spinner
    powertop
    qutebrowser
    redshift
    ristretto
    setroubleshoot
    setxkbmap
    thunderbird
    tlp
    tmux
    torbrowser-launcher
    youtube-dl

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
    "Common NetworkManager Submodules"
    "Hardware Support"
    "Development Tools"
    "C Development Tools and Libraries"
    "Virtualization"
)

sudo dnf install -y "${pkgs[@]}"
sudo dnf group install -y "${groups[@]}"

systemctl enable --now --user pipewire.service
systemctl enable --now --user pipewire-pulse.service
sudo systemctl enable --now docker
sudo systemctl enable --now libvirtd
sudo systemctl enable --now tlp
