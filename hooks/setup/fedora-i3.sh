#!/usr/bin/bash

pkg_groups=(
    base-x
)

pkgs=(
    dunst
    feh
    i3-gaps
    picom
    polybar
    redshift
    rofi
    rofi-wayland
    setxkbmap
    xss-lock
)

sudo dnf group install -y "${pkg_groups[@]}"
sudo dnf install -y "${pkgs[@]}"
