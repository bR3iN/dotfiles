#!/usr/bin/bash

sudo zypper addrepo --refresh --name "X11:Wayland" \
    "https://download.opensuse.org/repositories/X11:Wayland/openSUSE_Tumbleweed/X11:Wayland.repo"

echo a | sudo zypper refresh

pkgs=(
    bat
    blueberry
    fzf
    cargo
    d-feet
    wl-clipboard
    wlsunset
    docker
    firefox
    fish
    gnome-keyring
    greetd
    thunderbird
    gtkgreet
    kitty
    libnotify-tools
    mako
    mpv
    mpv-mpris
    neofetch
    neovim
    npm
    opi
    pipewire
    pipewire-pulseaudio
    qutebrowser
    seahorse
    secret-tool
    sway
    thunar
    tmux
    upower
    waybar
    swaylock
    wob
    zathura
    zathura-plugin-djvu
    zathura-plugin-ps

    # Required by packages above
    libqt5-qtwayland # Needed by qutebrowser on wayland
    libstdc++-devel  # Needed by treesitter(?)
)

sudo zypper install -y "${pkgs[@]}"

systemctl enable --user --now pipewire
systemctl enable --user --now pipewire-pulse

sudo systemctl set-default graphical.target

sudo systemctl enable --now docker
sudo systemctl enable       greetd
