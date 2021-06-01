#!/usr/bin/bash
set -e
set -u

INSTALLER_DIR=${INSTALLER_DIR-$(dirname $(dirname $(dirname $(readlink -f "$0"))))}
SCHEMA="org.gnome.settings-daemon.plugins.media-keys"
DCONF_PATH="/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings"

function main {
    while [[ "${1-}" =~ ^- ]]; do
        case "${1-}" in
            --reset-all)
                echo "Resetting all custom keybindings"
                reset
                ;;

            -f|--file)
                if [[ ! -z "$2" ]]; then
                    CONF="$2"
                    shift
                else
                    echo "Option '-f' is missing a parameter, exiting..."
                    return 1
                fi
                ;;
        esac
        shift
    done

    local count=$(get_count)
    local config=$(parse_config "${CONF-gnome/custom_keybindings.ini}")
    echo "Adding the following keybindings:"

    local IFS=$'\n'
    for line in $config; do 
        local -a binding
        IFS=':' read -a binding <<< "$line"
        add_keybinding "$count" "${binding[@]}" && let count++ || true
    done
}

function reset {
   dconf reset -f "$DCONF_PATH/"
   gsettings set "$SCHEMA" custom-keybindings "[]"
}

function dconf_dump {
    dconf dump "$DCONF_PATH/"
}

function get_count {
    local current_count=$(echo "${DCONF_DUMP:="$(dconf_dump)"}" \
        | grep '\[custom' \
        | tail -1 \
        | sed 's/\[custom\([0-9]*\)\]/\1/g')

    if [ -z "$current_count" ]; then
        echo 0
    else
        echo $(expr ${current_count} + 1)
    fi
}

function add_keybinding {
    local count=$1
    if ! grep -q "command=.*$4" <<< "${DCONF_DUMP:="$(dconf_dump)"}"; then
        echo "$2"

        gsettings set "${SCHEMA}.custom-keybinding:$DCONF_PATH/custom$count/" name    "'$2'"
        gsettings set "${SCHEMA}.custom-keybinding:$DCONF_PATH/custom$count/" binding "'$3'"
        gsettings set "${SCHEMA}.custom-keybinding:$DCONF_PATH/custom$count/" command "'$4'"

        local current_list="$(gsettings get $SCHEMA custom-keybindings)"
        new_list="$(echo "$current_list" \
            | awk -v new_entry="'$DCONF_PATH/custom$count/'" \
            '{sub("\\]$", ", "new_entry"]"); print}' \
            | awk '{sub("\\[, ","["); print}' )"

        gsettings set "$SCHEMA" custom-keybindings "$new_list"
        return 0
    else
        return 1
    fi
}

function parse_config {
    awk -f - "$INSTALLER_DIR/$1" << EOF
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

main $@
