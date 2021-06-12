#!/usr/bin/bash

sudo dnf copr enable -y agriffis/neovim-nightly &>> /dev/null

#gvfs
pkgs=(
    awesome
    bat
    blueberry
    firefox
    fish
    google-noto-emoji-color-fonts
    ImageMagick
    kitty
    lightdm-gtk
    lightdm-gtk-greeter-settings
    neovim
    NetworkManager-tui
    npm
    parole
    pavucontrol
    picom
    powertop
    pragha
    qutebrowser
    redshift
    ristretto
    rofi
    setroubleshoot
    setxkbmap
    Thunar
    thunar-volman
    thunderbird
    tlp
    tmux
    tumbler
    volumeicon
    xfce-polkit
    xfce4-power-manager
    xfce4-settings
    xfce4-taskmanager
    xfconf
    xset
    xss-lock
    zathura
    zathura-pdf-mupdf

    # For building the pop-os gtk themes and rofi-emoji
    meson
    ninja-build
    sassc
    glib2-devel
    cairo-devel
    rofi-devel
)

# Groups to install on top of group 'Fedora Server Edition'
groups=(
    "Core"
    "Standard"
    "Multimedia"
    "Printing Support"
    "base-x"
    "Hardware Support"
    "Development Tools"
    "C Development Tools and Libraries"
)

sudo dnf install -y "${pkgs[@]}"
sudo dnf group install -y "${groups[@]}"
