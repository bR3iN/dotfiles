#!/bin/bash

BATPATH=/sys/class/power_supply/BAT0
STATUS=$(cat "${BATPATH}/status")
FULL=$(cat "${BATPATH}/energy_full")
NOW=$(cat "${BATPATH}/energy_now")
CURRENT=$(( 100 * ${NOW} / ${FULL} ))

echo "${CURRENT} ${STATUS}"
