#!/usr/bin/bash

sudo dnf install -y https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm

pkg_groups=(
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

pkgs=(
    bat
    d-feet
    docker
    fedora-workstation-backgrounds
    fedora-workstation-repositories
    ffmpeg
    firefox
    fish
    ImageMagick
    imv
    jq
    kde-connect
    kitty
    mpv
    mpv-mpris
    neofetch
    neovim
    npm
    plymouth-theme-spinner
    power-profiles-daemon
    toolbox
    qutebrowser
    setroubleshoot
    setxkbmap
    tealdeer
    thunderbird
    tmux
    torbrowser-launcher
    xdotool #for vimtex
    youtube-dl

    # Order matters
    zathura
    zathura-pdf-poppler
    zathura-plugins-all

    # abcde
    # lame
    # mp3gain
    # python3-eyed3
    # lsscsi
)

sudo dnf group install -y "${pkg_groups[@]}"
sudo dnf install -y "${pkgs[@]}"

systemctl enable --now --user pipewire.service
systemctl enable --now --user pipewire-pulse.service
sudo systemctl enable --now docker
sudo systemctl enable --now libvirtd
sudo systemctl disable systemd-networkd-wait-online
sudo systemctl mask    systemd-networkd-wait-online

sudo plymouth-set-default-theme -R spinner
