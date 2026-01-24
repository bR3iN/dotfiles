#!/usr/bin/bash

swaymsg -t subscribe '["window"]' -m \
    | jq -c --unbuffered -r 'select(.change == "focus") | .container.app_id'
