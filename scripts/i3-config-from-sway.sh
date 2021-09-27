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
    BEGIN          { in_sway_block = 0 }
    /#sway-only\(/ { in_sway_block = 1; next }
    /#)/           { in_sway_block = 0; next }
    in_sway_block || /#sway-only/ { next }

    { sub("#i3: ",""); print }'
}

get_configs | filter > "${1:-/tmp/i3.config}"
