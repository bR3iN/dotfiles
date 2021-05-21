#!/usr/bin/bash

pkgs=(
    npm
    awesome
    rofi
)

sudo dnf install "${pkgs[@]}"
