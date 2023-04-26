#!/usr/bin/bash

if ! which flatpak &>/dev/null; then
    echo "ERROR: Flatpak not installed"
    exit 1
fi

# Install gradience
flatpak install com.github.GradienceTeam.Gradience

# Install adw-gtk3
flatpak install org.gtk.Gtk3theme.adw-gtk3 org.gtk.Gtk3theme.adw-gtk3-dark

if [ -d "/usr/share/themes/adw-gtk3" ]; then
    echo "adw-gtk3 is already installed"
else
    TMP_PATH=$(mktemp -u)
    wget -O "$TMP_PATH" "https://github.com/lassekongo83/adw-gtk3/releases/download/v4.5/adw-gtk3v4-5.tar.xz"
    sudo mkdir -p /usr/share/themes
    sudo tar xaf "$TMP_PATH" --directory=/usr/share/themes
    rm "$TMP_PATH"
fi

sudo flatpak run --command=gradience-cli com.github.GradienceTeam.Gradience flatpak-overrides -e both
sudo flatpak override --filesystem=xdg-config/gtk-4.0
sudo flatpak override --filesystem=xdg-config/gtk-3.0

# Set adw-gtk3 as theme
gsettings set org.gnome.desktop.interface gtk-theme 'adw-gtk3'
sudo gsettings set org.gnome.desktop.interface gtk-theme 'adw-gtk3'
