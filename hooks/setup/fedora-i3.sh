#!/usr/bin/bash

pkg_groups=(
    base-x
)

pkgs=(
    blueberry
    brightnessctl
    dunst
    feh
    gnome-keyring
    i3-gaps
    lxappearance
    NetworkManager-tui
    pavucontrol
    picom
    playerctl
    polybar
    pulseaudio-utils
    redshift
    rofi-wayland
    seahorse
    Thunar
    thunar-volman
    tumbler
    xfce-polkit
    xss-lock
)

sudo dnf group install -y "${pkg_groups[@]}"
sudo dnf install -y "${pkgs[@]}"

sudo systemctl enable greetd
