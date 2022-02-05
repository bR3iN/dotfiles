#!/bin/bash

BATPATH=/sys/class/power_supply/BAT0

if [ "$1" = '-q' ]; then
    [ -d "$BATPATH" ]
    exit "$?"
fi

STATUS=$(cat "${BATPATH}/status")
FULL=$(cat "${BATPATH}/energy_full")
NOW=$(cat "${BATPATH}/energy_now")
CURRENT=$(( 100 * ${NOW} / ${FULL} ))

if [ "$STATUS" == "Charging" ]; then
    echo " ${CURRENT}%[] "
else
    echo " ${CURRENT}% "
fi
