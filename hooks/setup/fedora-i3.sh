#!/usr/bin/bash

pkg_groups=(
    base-x
)

pkgs=(
    dunst
    feh
    i3
    lightdm
    lightdm-gtk
    picom
    polybar
    redshift
    rofi
    setxkbmap
    xss-lock
)

sudo dnf group install -y "${pkg_groups[@]}"
sudo dnf install -y "${pkgs[@]}"
