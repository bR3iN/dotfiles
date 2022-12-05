#!/usr/bin/bash
pkexec bash <<< "
if [ -d /var/lib/greetd ]; then
    mkdir -p /var/lib/greetd/.config/gtk-3.0
    cp \"$HOME/.config/gtk-3.0/gtk.css\" /var/lib/greetd/.config/gtk-3.0/
fi
for wallpaper in current current_blurred; do
    if [ -f \"$HOME/Wallpaper/\$wallpaper\" ]; then
        cp \"$HOME/Wallpaper/\$wallpaper\" /usr/share/backgrounds/
    fi
done
"
