#!/usr/bin/bash

sudo dnf module enable -y sway:rolling
sudo dnf copr   enable -y alebastr/sway-extras

pkgs=(
    greetd
    greetd-gtkgreet
    mako
    rofi-wayland
    sway-git
    swayidle
    swaylock
    waybar
    wl-clipboard
    wlsunset
    wob
)

# sudo dnf group install -y "${pkg_groups[@]}"
sudo dnf install -y "${pkgs[@]}"

systemctl --user enable --now mako.service
