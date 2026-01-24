#!/usr/bin/bash
pkexec bash <<< "
if [ -d /var/lib/greetd ]; then
    for dir in gtk-3.0 gtk-4.0; do
        mkdir -p /var/lib/greetd/.config/\$dir
        cp \"$HOME/.config/\$dir/gtk.css\" /var/lib/greetd/.config/\$dir/
    done
fi

for wallpaper in current current_blurred; do
    if [ -f \"$HOME/Wallpaper/\$wallpaper\" ]; then
        cp \"$HOME/Wallpaper/\$wallpaper\" /usr/share/backgrounds/
    fi
done
"
