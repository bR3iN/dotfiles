#!/bin/bash

function main {
    if [ "$2" == "Charging" ]; then
        echo "[${1}%]"
    else
        echo " ${1}% "
    fi
}

main $($HOME/.local/scripts/battery.sh)
