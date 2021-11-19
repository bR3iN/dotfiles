#!/usr/bin/bash

get_configs()
{
    cat \
        ~/.config/sway/flavours \
        ~/.config/sway/config \
        ~/.config/sway/config.d/* \
        2> /dev/null || true
}

filter()
{
    awk '\
    BEGIN             { ws = "( |    )*"; in_sway_block = 0; skip_next = 0 }
    /^( |   )*#i3-skip-next:/ { skip_next = 1; next }
    /^( |   )*#sway-only\(/   { in_sway_block = 1; next }
    /^( |   )*#)/             { in_sway_block = 0; next }

    in_sway_block || /#sway-only/ { next }
    skip_next { skip_next = 0; next }

    /^( |   )*#i3-change-next-to: / {
        skip_next = 1
        sub("^( |   )*#i3-change-next-to: ", "")
        print
        next
    }

    { sub("^( | )*#i3: ",""); print }'
}

get_configs | filter > "${1:-/tmp/i3.config}"
