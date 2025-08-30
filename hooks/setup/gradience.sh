#!/usr/bin/bash

set -e

if ! which flatpak &>/dev/null; then
    echo "ERROR: Flatpak not installed"
    exit 1
fi

# Install gradience
sudo flatpak install -y com.github.GradienceTeam.Gradience

flatpak run --command=gradience-cli com.github.GradienceTeam.Gradience flatpak-overrides -e both

if [ -n "$HOME/.dotfiles" ]; then
    # Allows following symlinks into the dotfile directory
    sudo flatpak override --filesystem="$HOME/.dotfiles" com.github.GradienceTeam.Gradience
fi

# Install adw-gtk3
# Doesn't currently work for some reason, instead we expose home/.themes below.
# Could be related to https://github.com/lassekongo83/adw-gtk3/issues/235
# sudo flatpak install -y org.gtk.Gtk3theme.adw-gtk3 org.gtk.Gtk3theme.adw-gtk3-dark

if [ -d "/usr/share/themes/adw-gtk3" ] && [ -d "$HOME/.themes/adw-gtk3" ]; then
    echo "adw-gtk3 is already installed"
else
    TMP_PATH=$(mktemp -u)
    wget -O "$TMP_PATH" "https://github.com/lassekongo83/adw-gtk3/releases/download/v4.5/adw-gtk3v4-5.tar.xz"

    if [ ! -d "/usr/share/themes/adw-gtk3" ]; then
        echo "Installing adw-gtk3 system-wide"
        sudo mkdir -p /usr/share/themes
        sudo tar xaf "$TMP_PATH" --directory=/usr/share/themes
    fi
    # Flatpak can't expose /usr/share/themes and flatpak theme doesn't work that well (TODO), so install also into ~/.themes
    if [ ! -d "$HOME/.themes/adw-gtk3" ]; then
        echo "Installing adw-gtk3 for the current user"
        mkdir -p "$HOME/.themes"
        tar xaf "$TMP_PATH" --directory="$HOME/.themes"
    fi
    rm "$TMP_PATH"
fi

# Set adw-gtk3 as theme for the user
gsettings set org.gnome.desktop.interface gtk-theme 'adw-gtk3'

# Gradience doesn't create those automatically
mkdir -p ~/.config/gtk-{4,3}.0

# Allow flatpak apps to read color overwrites and in particular Gradience to write them
sudo flatpak override --filesystem=home/.themes
sudo flatpak override --filesystem=xdg-config/gtk-4.0
sudo flatpak override --filesystem=xdg-config/gtk-3.0
sudo flatpak override --env=GTK_THEME=adw-gtk3
