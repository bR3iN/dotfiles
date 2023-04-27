#!/usr/bin/bash

# See https://linuxnightly.com/install-firefox-ubuntu/
echo "Package: *
Pin: release o=LP-PPA-mozillateam
Pin-Priority: 1002" \
    | sudo tee /etc/apt/preferences.d/firefox > /dev/null

sudo add-apt-repository -y ppa:neovim-ppa/unstable ppa:mozillateam/ppa
sudo apt update
sudo apt upgrade -y
sudo apt remove -y snapd

pkgs=(
    # Workstation
    apt-file
    bat
    blueberry
    brightnessctl
    cmus
    firefox
    fish
    flatpak
    gcc
    git
    imagemagick
    kitty
    make
    mpv
    mpv-mpris
    neovim
    network-manager
    npm
    pavucontrol
    playerctl
    qutebrowser
    rofi
    teeldear
    thunar
    tmux
    torbrowser-launcher
    xdotool
    xorg
    zip

    # DE specific
    xfce-polkit
    mako-notifier
    pipewire
    waybar
    xwayland
)


sudo apt install -y "${pkgs[@]}"
sudo apt install --no-install-recommends -y sway

sudo add-apt-repository -y ppa:neovim-ppa/unstable
sudo apt update
sudo apt install -y neovim

systemctl enable --now --user pipewire.service
systemctl enable --now --user pipewire-pulse.service
# sudo systemctl set-default graphical.target
