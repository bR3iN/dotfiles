#!/usr/bin/bash

gsettings set org.gnome.mutter overlay-key ''
gsettings set org.gnome.desktop.input-sources xkb-options "['altwin:prtsc_rwin']"
gsettings set org.gnome.settings-daemon.plugins.media-keys logout "[]"
gsettings set org.gnome.desktop.wm.keybindings switch-input-source "['<Super>i']"
gsettings set org.gnome.desktop.wm.keybindings switch-input-source-backward "['<Shift><Super>i']"
gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'us'), ('xkb', 'us+intl')]"
