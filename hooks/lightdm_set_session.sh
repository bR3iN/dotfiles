#!/usr/bin/bash

CONF=/etc/lightdm/lightdm.conf

if [ -f "$CONF" ] && [ -n "$1" ]; then
    cat "$CONF" | sed 's/#\?\(user-session=\).*/\1'"$1"'/' | sudo tee "$CONF" >> /dev/null
fi
