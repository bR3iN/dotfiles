#!/usr/bin/bash

sudo dnf module enable -y sway:rolling
sudo dnf copr   enable -y alebastr/sway-extras

pkgs=(
    blueberry
    brightnessctl
    gnome-keyring
    greetd
    greetd-gtkgreet
    mako
    NetworkManager-tui
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

# sudo dnf group install -y "${pkg_groups[@]}"
sudo dnf install -y "${pkgs[@]}"

systemctl --user enable --now mako.service
sudo systemctl enable greetd
