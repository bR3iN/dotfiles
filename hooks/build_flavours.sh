#!/usr/bin/bash

URL="https://github.com/Misterio77/flavours"

build_and_install ()
{
    mkdir -p ~/Github
    git clone "$URL" ~/Github/flavours
    cd ~/Github/flavours
    cargo install flavours
    ~/.cargo/bin/flavours update all &
}

if [ ! -d ~/Github/flavours ]; then
    build_and_install
fi
