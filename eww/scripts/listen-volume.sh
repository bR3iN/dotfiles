#!/usr/bin/bash

get_vol() {
    pactl get-sink-volume @DEFAULT_SINK@ | awk '/Volume/{ gsub("%", "", $5); printf $5 }'
}

get_mute() {
    pactl get-sink-mute @DEFAULT_SINK@ \
        | awk '{ if ($2 == "yes") { printf "true" } else { printf "false" }}'
}

print() {
    echo "{\"volume\": \"$(get_vol)\", \"muted\": $(get_mute)}"
}

print
pactl subscribe \
    | grep --line-buffered "Event 'change' on sink" \
    | while read; do print; done
