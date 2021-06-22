#!/usr/bin/bash

URL="https://raw.githubusercontent.com/Alexays/Waybar/master/resources/custom_modules/mediaplayer.py"

if [ ! -f ~/.config/waybar/mediaplayer.py ]; then
    wget -O ~/.config/waybar/mediaplayer.py "$URL"
fi
