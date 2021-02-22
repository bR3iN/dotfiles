#!/bin/bash

BATPATH=/sys/class/power_supply/BAT1
STATUS=$(cat "${BATPATH}/status")
FULL=$(cat "${BATPATH}/energy_full")
NOW=$(cat "${BATPATH}/energy_now")
CURRENT=$(( 100 * ${NOW} / ${FULL} ))

if [ "x${STATUS}" == "xCharging" ]; then
	echo "{${CURRENT}%}"
else
	echo "[${CURRENT}%]"
fi

