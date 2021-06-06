#!/usr/bin/bash
set -o errexit
set -o nounset
set -o pipefail

export INSTALLER_DIR=$(dirname $(readlink -f "$0"))
CONF="${INSTALLER_DIR}/dotfiles.ini"
OUTPUT_SEPARATOR='='

declare -A INSTALLED

function main {
    while [[ "${1-}" =~ ^- ]]; do
        case "${1-}" in
            -c|--copy)
                NO_SYMLINKS="true"
                ;;

            -y|--always-yes)
                ALWAYS_YES="true"
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
    (awk -f - "$CONF" | sort) << EOF
    /^\[.*\]/ {
    gsub(/(\[( |\t)*|( |\t)*\])/,"")
    print
}
EOF
exit
}


function installTarget {
    local target="$1"
    if [ ! -z "${INSTALLED[$target]-}" ]; then
        return 0
    else
        INSTALLED["$target"]="true"
    fi

    local keys=$(getKeys "$target")
    if [ -z "$keys" ]; then
        echo "Section '$target' was not found or contains no keys."
        return 1
    fi

    local IFS=$'\n'
    for key in $keys; do
        local IFS=' '

        #split `key` into name and value
        IFS="$OUTPUT_SEPARATOR" read -ra _key <<< "$key"
        local name="${_key[0]-}"
        local value="${_key[1]-}"

        if [[ -z "$name" ]] || [[ -z "$value" ]]; then
            echo "WARNING: '$key' is not a valid key."
            continue
        fi

        if [[ ! "$name" =~ ^@ ]]; then
            createSymlink "$name" "$value"
        else
            case "${name#[@]}" in
                cmd)
                    runCommand "$value"
                    ;;

                hook)
                    runHook "$value"
                    ;;

                target)
                    installTarget "$value"
                    ;;
                install)
                    installFile "$value"
            esac
        fi
    done
}


function runHook {
    echo "Running hook '$1'"
    local hook_cmd="${INSTALLER_DIR}/hooks/$1"
    eval "($hook_cmd)" 2>&1 | sed "s/^/    /" || local exit_code="$?"
    if [ ! "${exit_code:-0}" -eq 0 ]; then
        echo "ERROR: Hook '$1' failed with exit code ${exit_code}"
    fi
}


function runCommand {
    echo "Running command '$1'"
    eval "($1)" 2>&1 | sed "s/^/    /" || local exit_code="$?"
    if [ ! "${exit_code:-0}" -eq 0 ]; then
        echo "ERROR: Command '$1' failed with exit code ${exit_code}"
    fi
}


function installFile {
    local file="$1"

    if [[ "$file" =~ ^root: ]]; then
        file="${file#root:}"
        as_root=true
    fi

    if [ -f "$INSTALLER_DIR/$file" ]; then
        if [ -n "${as_root-}" ]; then
            sudo install -D -m=0755 -t "/usr/local/bin" "$INSTALLER_DIR/$file"
        else
            createSymlink "$file" "$HOME/.local/bin/$(basename $file)"
        fi
    elif [ -d "$INSTALLER_DIR/$file" ]; then
        (cd "$INSTALLER_DIR/$file"; ${as_root:+sudo} make install)
    fi
}


function createSymlink {
    local dest="${INSTALLER_DIR}/$1"
    if [ ! -e "$dest" ]; then
        echo "WARNING: Link destination '${dest}' was not found. Skipping..."
        return 0
    fi

    if [[ "$2" =~ ^root: ]]; then
        local as_root=sudo
        local path=$(getPath "${2#root:}")
    else
        local path=$(getPath "$2")
    fi

    if [ -n "${NO_SYMLINKS-}" ] || [ -n "${as_root-}" ]; then
        local cmd='cp -rT' 
        local on_success='Created the copy'
    else
        local cmd='ln -sT'
        local on_success='Created the symbolic link'
    fi

    if ${as_root-} [ -h "$path" ]; then
        ${as_root-} rm -f "$path" || local exit_code=$?
    elif ${as_root-} [ -e "$path" ]; then
        confirmAndDelete "$path" ${as_root-} || local exit_code=$?
    fi

    if [ "${exit_code:-0}" -eq 0 ]; then
        ${as_root:+sudo} mkdir -p "$(dirname "$path")"
        ${as_root:+sudo} $cmd "$dest" "$path"

        if [ "$?" -eq 0 ]; then
            echo "Created the symbolic link or copy"
            echo "    $path --> $dest"
        fi
    fi
}


function confirmAndDelete {
    # $1 is path of file/dir to remove; $2 is 'sudo' or empty

    if [ "${ALWAYS_YES-}" = true ]; then
        ${2-} rm -rf "$1" && return 0
        return 1
    fi

    echo "WARNING: The file/directory"
    echo
    echo "  '$1'"
    echo
    echo -n "already exists. Do you want to delete it? (y|N) "

    while true; do
        read confirm
        case "$confirm" in
            y|Y) ${2-} rm -rf "$1" && return 0
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
    if [[ "$1" =~ ^~/ ]]; then
        echo "$HOME/${1#"~/"}"
    elif [[ ! "$1" =~ ^/ ]]; then
        echo "$HOME/$1"
    else
        echo "$1"
    fi
}


function getKeys {
    awk -F= -v TARGET_SECTION="$1" \
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
        print \$1 "$OUTPUT_SEPARATOR" \$2
    }

# Matches target header
\$0 ~ "^\\\\[" ws TARGET_SECTION ws"\\\\]" \
    {
        in_target_section = 1
    }
EOF
}


main "$@"
