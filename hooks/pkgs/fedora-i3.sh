#!/usr/bin/bash

pkgs=(
    blueberry
    xss-lock
    brightnessctl
    dunst
    feh
    gnome-keyring
    greetd
    greetd-gtkgreet
    i3-gaps
    lxappearance
    NetworkManager-tui
    pavucontrol
    picom
    playerctl
    polybar
    pulseaudio-utils
    rofi-wayland
    seahorse
    Thunar
    thunar-volman
    tumbler
    xfce-polkit
)

sudo dnf group install -y base-x
sudo dnf install -y "${pkgs[@]}"

sudo systemctl enable greetd
