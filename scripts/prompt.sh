#!/usr/bin/bash

set -e
set -u

for cmd in list_options callback; do
    if [ "$(type -t "$cmd")" != function ]; then
        echo "Error: Invalid usage of 'prompt.sh': $cmd is not defined"
        exit 1
    fi
done

if [ -n "${WAYLAND_DISPLAY-}" ] && which wofi >/dev/null; then
    CMD=(
        wofi
        --dmenu
        --prompt "${PROMPT-}"
        "${WOFI_OPTIONS[@]}"
    )
else
    CMD=(
        rofi
        -dmenu -i
        -p "${PROMPT-}"
        -no-show-icons
        -sorting-method levenshtein
        -matching normal
        -no-fixed-num-lines
        "${ROFI_OPTIONS[@]}"
    )
fi

selection="$(list_options | "${CMD[@]}")"

if [ -n "$selection" ]; then
    callback "$selection"
fi
