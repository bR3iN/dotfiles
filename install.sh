#!/bin/bash
set -o errexit
set -o nounset
set -o pipefail

INSTALLER_DIR=$(dirname $(readlink -f "$0"))
CONF="${INSTALLER_DIR}/dotfiles.ini"

declare -A INSTALLED

function main {
    while [[ "${1-}" =~ ^- ]]; do
        case "${1-}" in
            -c|--copy)
                NO_SYMLINKS="true"
                ;;

            -l|--list)
                listTargets
                exit
                ;;

            -h|--help)
                usage
                exit
                ;;

            -f|--file)
                if [[ ! -z "$2" ]]; then
                    CONF="$2"
                    shift
                else
                    usage
                    exit
                fi
                ;;
        esac
        shift
    done

    if [[ -z "${1+x}" ]]; then
        usage
        exit
    fi

    for target in "$@"; do
        installTarget "$target"
    done
}

function usage {
    echo "usage... WIP"
}

function listTargets {
    echo "Available targets:"
    echo "=================="
    awk -f - "$CONF" << EOF
    /^\[.*\]/ {
    gsub(/(\[( |\t)*|( |\t)*\])/,"")
    print
}
EOF
exit
}

function installTarget {
    local target="$1" # allows for recursion

    if [ ! -z "${INSTALLED["$target"]-}" ]; then
        return 0
    else
        INSTALLED["$target"]="true"
    fi

    local keys=$(getKeys)
    if [ -z "$keys" ]; then
        echo "Section '$target' was not found or contains no keys."
        return 1
    fi

    local IFS=$'\n'
    for key in $keys; do

        #split `key` into name and value
        IFS=":" read -ra _key <<< "$key"
        local name="${_key[0]-}"
        local value="${_key[1]-}"

        if [[ -z "$name" ]] || [[ -z "$value" ]]; then
            echo "WARNING: '${name} = ${value}' is not a valid key."
            continue
        fi

        if [[ ! "$name" =~ ^@ ]]; then
            createSymlink
        else
            case "${name#[@]}" in
                cmd)
                    echo "Running command '${value}'"
                    eval "$value" 2>&1 | sed "s/^/    /" || local exit_code="$?"
                    if [ ! "${exit_code:-0}" -eq 0 ]; then
                        echo "ERROR: Command '${value}' failed with exit code ${exit_code}"
                    fi
                    ;;

                hook)
                    echo "Running hook '${value}'"
                    ("${INSTALLER_DIR}/hooks/${value}") 2>&1 | sed "s/^/    /" \
                        || local exit_code="$?"
                    if [ ! "${exit_code:-0}" -eq 0 ]; then
                        echo "ERROR: Hook '${value}' failed with exit code ${exit_code}"
                    fi
                    ;;

                target)
                    installTarget "$value"
                    ;;
            esac
        fi
    done
}


function createSymlink {

    # parses `value` and defines `path`
    getPath

    dest="${INSTALLER_DIR}/${name}"

    if [ ! -e "$dest" ]; then
        echo "WARNING: Link destination '${dest}' was not found. Skipping..."
        return 0
    fi

    if [ -h "$path" ]; then
        rm -f "$path" || local exit_code=$?
    elif [ -e "$path" ]; then
        confirmAndDelete || local exit_code=$?
    fi

    if [ "${exit_code:-0}" -eq 0 ]; then
        mkdir -p "$(dirname "$path")"
        ln -sT "$dest" "$path"

        if [ "$?" -eq 0 ]; then
            echo "Created the symbolic link"
            echo "    $path --> $dest"
        fi
    fi
}


function confirmAndDelete {
    echo "WARNING: The file/directory"
    echo
    echo "  '$path'"
    echo
    echo -n "already exists. Do you want to delete it? (y|N) "

    while true; do
        read confirm
        case "$confirm" in
            y|Y) rm -rf "$path" && return 0
                return 1
                ;;
            n|N)
                return 1
                ;;
            "")
                return 1
                ;;
            *)
                echo "Please anwser 'y' or 'n'."
                ;;
        esac
    done
}


function getPath {
    if [[ "$value" =~ ^~/ ]]; then
        path="${HOME}/${value#"~/"}"
    elif [[ ! "$value" =~ ^/ ]]; then
        path="${HOME}/${value}"
    else
        path="$value"
    fi
}


function getKeys {
    awk -F= -v TARGET_SECTION="$target" \
        -f - "$CONF" << EOF
            BEGIN {
            in_target_section = 0
            ws = "( |\t)*"
        }

# Matches non-target header
/^\[.*\]/ \
    && \$0 !~ "^\\\\[" ws TARGET_SECTION ws "\\\\]" \
    {
        in_target_section = 0
    }

in_target_section \
    && \$0 !~ "^" ws ";" \
    && \$0 ~ ws "[^ \t]" \
    {
        gsub(/(^(\t| )+|(\t| )+\$)/,"",\$1)
        gsub(/(^(\t| )+|(\t| )+\$)/,"",\$2)
        print \$1 ":" \$2
    }

# Matches target header
\$0 ~ "^\\\\[" ws TARGET_SECTION ws"\\\\]" \
    {
        in_target_section = 1
    }
EOF
}

main "$@"
