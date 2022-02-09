#!/usr/bin/bash

set -e
set -u


is_function() {
    [ "$(type -t "$1")" = function ]
}


check_config()
{
    if ! is_function list_options || ! is_function callback; then
        exit 1
    fi
}


prompt()
{
    if [[ "$(basename "$0")" =~ ^wofi ]]; then
        CMD=(
            wofi
            --dmenu
            --insensitive
            --prompt "${PROMPT-}"
            --matching contains
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
    list_options | "${CMD[@]}"
}


check_config

selection="$(prompt)"

if [ -n "$selection" ]; then
    callback "$selection"
else
    exit 0
fi
