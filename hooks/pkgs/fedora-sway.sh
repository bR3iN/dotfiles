#!/usr/bin/bash

sudo dnf module enable -y sway:rolling
sudo dnf copr   enable -y alebastr/sway-extras

pkgs=(
    blueberry
    brightnessctl
    gnome-keyring
    greetd
    greetd-gtkgreet
    lxappearance
    NetworkManager-tui
    nnn
    pavucontrol
    playerctl
    pulseaudio-utils
    rofi-wayland
    seahorse
    sway-git
    swayidle
    swaylock
    Thunar
    thunar-volman
    tumbler
    waybar
    wl-clipboard
    wlsunset
    wob
    xfce-polkit
)

sudo dnf install -y "${pkgs[@]}"
sudo dnf group install -y "${groups[@]}"

sudo systemctl enable greetd
