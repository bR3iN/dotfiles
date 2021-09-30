#!/usr/bin/bash

CONFIG=~/.config/rofi/scripts.ini
LOG=/tmp/rofi-scripts.log

if [ -z "$1" ]; then
    echo -en "\0nocustom\x1ftrue\n"
    awk '/^\[.*\]$/ { gsub(/(\[|\])/,""); print}' "$CONFIG"
else
    declare -a target
    local path=$(awk -v target="$1" -F "=" '
    # BEGIN { in_target = 0 }

    /^\[.*\]$/ {
        if ($0 ~ "^\\["target"\\]$") {
            in_target = 1
        } else {
            in_target = 0
        }
    }

    /^path/ && in_target {
        gsub(/(^ +| +$)/,"", $2)
        print $2
        exit
    }' "$CONFIG")

    if [ -n "$path" ]; then
        notify-send -t 3000 "$1" "started"
        eval "(${path})" &> /dev/null &
    fi
fi
