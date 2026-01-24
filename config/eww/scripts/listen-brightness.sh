#!/usr/bin/bash

MAX_BRIGHTNESS="$(brightnessctl max)"

get() {
    local current="$(brightnessctl get)"
    echo $((100 * "$current" / "$MAX_BRIGHTNESS"))
}

get

udevadm monitor --udev --property --subsystem-match=backlight \
    | grep --line-buffered ACTION=change \
    | while read;
do
    get
done
