#!/usr/bin/bash

if [ "$1" = on ]; then
    set() { git update-index --assume-unchanged "$@"; }
elif [ "$1" = off ]; then
    set() { git update-index --no-assume-unchanged "$@"; }
else
    echo "USAGE: ignore-base16.sh on|off"
    exit 1
fi

git_root=$(git rev-parse --show-toplevel 2>/dev/null)

if [ "$?" -ne 0 ]; then
    echo "ERROR: not in git repository"
    exit 128
fi

list_files() {
    sed -n 's/rewrite\s\+=\s\+"\(.*\)"/\1/p' ~/.config/base16-colorizer/targets.toml
}

mapfile -t files < <(list_files)

for file in "${files[@]}"; do
    canonicalized=$(readlink -f "${file/'~'/"$HOME"}")

    if [[ ! "$canonicalized" =~ ^"$git_root" ]]; then
        continue
    fi

    set "$canonicalized"
done
