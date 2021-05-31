#!/usr/bin/bash

gsettings set org.gnome.shell.extensions.pop-shell toggle-stacking-global "['<Shift><Super>s']"
gsettings set org.gnome.shell.extensions.pop-shell activate-launcher "[]"
gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'us'), ('xkb', 'us+intl')]"
