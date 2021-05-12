#!/usr/bin/bash
set -e

SCHEMA="org.gnome.settings-daemon.plugins.media-keys"
DCONF_PATH="/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings"

function reset {
   dconf reset -f "$DCONF_PATH/"
   gsettings set "$SCHEMA" custom-keybindings "[]"
}

function dconf_dump {
    dconf dump "$DCONF_PATH/"
}

function get_count {
    local current_count="$(echo "${DCONF_DUMP:="$(dconf_dump)"}" \
        | grep '\[custom' \
        | tail -1 | sed 's/\[custom\([0-9]*\)\]/\1/g')"

    if [ -n "$current_path" ]; then
        echo 0
    else
        echo "$(expr ${current_count} + 1)"
    fi
}

function add_keybinding {
    if ! grep -q "command=.*$3" <<< "${DCONF_DUMP:="$(dconf_dump)"}"; then
        
        echo "$1"

        gsettings set "${SCHEMA}.custom-keybinding:$DCONF_PATH/custom$COUNT/" name    "'$1'"
        gsettings set "${SCHEMA}.custom-keybinding:$DCONF_PATH/custom$COUNT/" binding "'$2'"
        gsettings set "${SCHEMA}.custom-keybinding:$DCONF_PATH/custom$COUNT/" command "'$3'"

        local current_list="$(gsettings get $SCHEMA custom-keybindings)"
        new_list="$(echo "$current_list" \
            | awk -v new_entry="'$DCONF_PATH/custom$COUNT/'" \
            '{sub("\\]$", ", "new_entry"]"); print}' \
            | awk '{sub("\\[, ","["); print}' )"

        gsettings set "$SCHEMA" custom-keybindings "$new_list"
    fi
}

function parse_config {
    awk -f - ~/.dotfiles/keybindings.ini << EOF
/\[.*\]/ {
    dump(name, binding, command)
    gsub(/(\[|\])/,"")
    name = \$0
    command = ""; binding = "";
}

/^command/ {
    gsub(/^command( |\t)*=( |\t)*/, "")
    command = \$0
}

/^binding/ {
    gsub(/^binding( |\t)*=( |\t)*/, "")
    binding = \$0
}

END {
    dump(name, binding, command)
}

function dump(name, binding, command){
    if (name) { print name":"binding":"command }
}
EOF
}

function main {
    if [ "$1" = "--reset-all" ]; then
        reset
    fi
    local COUNT="$(get_count)"
    local config="$(parse_config)"
    echo "Adding the following keybindings:"

    local IFS=$'\n'
    for line in $config; do 
        local -a binding
        IFS=':' read -a binding <<< "$line"
        add_keybinding "${binding[@]}"
        let COUNT++
    done
}

#main --reset-all
main $@
