#!/usr/bin/bash

payload="@import url(\"file://$HOME/.config/zen-themes/base16.css\");";

for file in ~/.zen/*; do
    if [ -d "$file" ]; then
        mkdir -p "$file/chrome"
        user_chrome="$file/chrome/userChrome.css"
        touch "$user_chrome"
        if ! grep -qe "$payload" "$user_chrome"; then
            echo "Injecting theme into $user_chrome"
            echo "$payload" >> "$user_chrome"
        fi
    fi
done
