#!/usr/bin/bash

get() {
    fmt_ws='"(workspace_button :focused \(.focused) :name \"\(.name)\" :urgent \(.urgent))"'
    swaymsg -r -t get_workspaces \
        | jq -r -c "map($fmt_ws) | join(\" \")"
}

get

swaymsg -t subscribe '["workspace"]' -m \
    | while read;
do
    get
done
