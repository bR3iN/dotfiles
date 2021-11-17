#!/usr/bin/bash

pkgs=(
    bat
    blueberry
    cargo
    d-feet
    dunst
    feh
    firefox
    fish
    fzf
    gnome-keyring
    i3-gaps
    i3lock
    kitty
    libnotify-tools
    mpv
    mpv-mpris
    neofetch
    neovim
    npm
    opi
    picom
    pipewire
    pipewire-pulseaudio
    polybar
    qutebrowser
    redshift
    rofi
    seahorse
    secret-tool
    swaylock
    thunar
    thunderbird
    tmux
    waybar
    xinit
    xorg-x11-server
    zathura
    zathura-plugin-djvu
    zathura-plugin-ps

    # Required by packages above
    libstdc++-devel  # Needed by treesitter(?)
)

sudo zypper install -y "${pkgs[@]}"

systemctl enable --user --now pipewire
systemctl enable --user --now pipewire-pulse
