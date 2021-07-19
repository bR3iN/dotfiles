#!/usr/bin/bash

gsettings set org.gnome.shell.extensions.pop-shell toggle-stacking-global "['<Control><Super>s']"
gsettings set org.gnome.shell.extensions.pop-shell activate-launcher "[]"
gsettings set org.gnome.settings-daemon.plugins.media-keys terminal "[]"

[ ! -f ~/Wallpaper/current ] && ~/Wallpaper/set pop-os
gsettings set org.gnome.desktop.background picture-uri "file:///$HOME/Wallpaper/current"
