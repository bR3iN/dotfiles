#!/usr/bin/bash

set -o errexit
set -o nounset
set -o pipefail


ERROR="\e[1;31m"
WARNING="\e[1;33m"
INFO="\e[0;32m"
CYAN="\e[0;36m"
BLUE="\e[0;34m"
PURPLE="\e[0;35m"
YELLOW="\e[0;33m"


export INSTALLER_DIR=$(dirname $(readlink -f "$0"))
export INSTALLER_USER="$(whoami)"

CONF="${INSTALLER_DIR}/dotfiles.ini"

declare -A INSTALLED


function main {
    while [[ "${1-}" =~ ^- ]]; do
        case "${1-}" in
            -c|--copy)
                NO_SYMLINKS=true
                ;;

            -d|--dry-run)
                DRY_RUN=true
                ;;

            -n|--no-delete) # Do not delete files or directories that aren't symlinks
                NO_DELETION=true
                ;;

            -y|--always-yes)
                ALWAYS_YES=true
                ;;

            -l|--list)
                listTargets
                exit
                ;;

            -f|--config-file)
                if [[ -n "${2-}" ]]; then
                    CONF="$2"
                    shift
                else
                    error "No config file provided."
                    exit 1
                fi
                ;;

            *)
                error "Unknown option '$1'."
                exit 1
                ;;
        esac
        shift
    done

    if [[ -z "${1+x}" ]]; then
        error "No targets provided."
        exit 1
    fi

    for target in "$@"; do
        installTarget "$target"
    done
}


function installTarget {
    local target="$1"

    if [ -z "$target" ]; then
        return
    elif [ -n "${INSTALLED[$target]-}" ]; then
        info "Target $(cwrap "$BLUE" "$target") is already installed. Skipping..."
        return 0
    else
        INSTALLED["$target"]="true"
        info "Installing target $(cwrap "$BLUE" "$target")."
    fi

    # Read lines into an array so we don't mess with the stdin inside the loop
    readarray lines < <(parseTarget "$target")
    for line in "${lines[@]}"; do
        IFS=$'\t' read type key value <<< "$line"

        case "$type" in
            links)
                createSymlink "$key" "$value"
                ;;
            hooks)
                case "$key" in
                    cmd)
                        runCommand "$value"
                        ;;
                    run)
                        runHook "$value"
                        ;;
                    *)
                        error "Unknown key '$key'."
                        exit 1
                        ;;
                esac
                ;;
            installs)
                case "$key" in
                    user)
                        userInstall "$value"
                        ;;
                    system)
                        systemInstall "$value"
                        ;;
                    *)
                        error "Unknown key '$key'."
                        exit 1
                        ;;
                esac
                ;;
            needs)
                case "$key" in
                    target)
                        installTarget "$value"
                        ;;
                    *)
                        error "Unknown key '$key'."
                        exit 1
                        ;;
                esac
                ;;
            *)
                error "Unknown type '$type'."
                exit 1
                ;;
        esac
    done
}


function is_dry_run {
    [ "${DRY_RUN-}" = true ]
    return "$?"
}


function runHook {
    info "Running hook $(cwrap "$PURPLE" "'$1'")."
    is_dry_run && return
    runIndented "${INSTALLER_DIR}/hooks/$1"
}


function runCommand {
    info "Running command $(cwrap "$PURPLE" "'$1'")."
    is_dry_run && return
    runIndented "$1"
}


function runIndented {
    bash -c "$1" 2>&1 | indentStdout || local exit_code="$?"
    pushd "$INSTALLER_DIR" &> /dev/null
    if [ ! "${exit_code:-0}" -eq 0 ]; then
        error "$(cwrap "$PURPLE" "'$*'") failed with exit code ${exit_code}."
    fi
    popd &> /dev/null
}


function indentStdout {
    sed 's/^/    /' | cpipe "$CYAN"
}


function userInstall {
    local file="$1"
    info "Installing $(cwrap "$PURPLE" "'$file'")."
    is_dry_run && return

    local path="$INSTALLER_DIR/$file"
    if [ -f "$path" ]; then
        createSymlink "$file" "$HOME/.local/bin/$(basename "$file")"
    elif [ -d "$path" ]; then
        (cd "$path"; make install) | indentStdout || local exit_code="$?"
        if [ ! "${exit_code:-0}" -eq 0 ]; then
            error "Install directive $(cwrap "$PURPLE" "'$file'") exited with code $?."
        fi
    fi

}


function systemInstall {
    local file="$1"
    info "Installing $(cwrap "$PURPLE" "'$file'")."
    is_dry_run && return

    local path="$INSTALLER_DIR/$file"
    if [ -f "$path" ]; then
        sudo install -D -m=0755 -t "/usr/local/bin" "$path"
    elif [ -d "$path" ]; then
        (cd "$path"; sudo --preserve-env=INSTALLER_USER make install) | indentStdout || local exit_code="$?"
        if [ ! "${exit_code:-0}" -eq 0 ]; then
            error "Install directive $(cwrap "$PURPLE" "'$file'") exited with code $?."
        fi
    fi
}


function createSymlink {
    local dest="${INSTALLER_DIR}/$1"
    local path="$(expandPath "$2")"

    if [ ! -e "$dest" ]; then
        warning "Link destination '${dest}' was not found. Skipping..."
        return 0
    fi

    if [ -z "${NO_SYMLINKS-}" ]; then
        local cmd=(ln -sT)
        info "Creating the symlink"
    else
        local cmd=(cp -rT)
        info "Creating the copy"
    fi
    echo -n "    "
    echo "$(cwrap "$PURPLE" "$path") $(cwrap "$YELLOW" "-->") $(cwrap "$PURPLE" "$dest")."

    is_dry_run && return

    if [ -h "$path" ]; then
        rm -f "$path"
    elif [ -e "$path" ]; then
        confirmAndDelete "$path" || return 0
    fi

    if [ "${exit_code:-0}" -eq 0 ]; then
        mkdir -p "$(dirname "$path")"
        "${cmd[@]}" "$dest" "$path"
    fi
}


function confirmAndDelete {
    local path="$1"

    if [ "${NO_DELETION-}" = true ]; then
        info "$(cwrap "$PURPLE" "'$path'") is not removed due to '-n|--no-deletion' option"
        return 1
    elif [ "${ALWAYS_YES-}" = true ]; then
        rm -rf "$path"
        return
    fi

    warning "The file/directory"
    echo
    echo "    $(cwrap "$PURPLE" "'$1'")"
    echo
    echo -n "already exists. Do you want to delete it? (y|N) "

    while true; do
        read confirm
        case "$confirm" in
            y|Y) rm -rf "$path"
                return
                ;;
            n|N)
                return 1
                ;;
            "")
                return 1
                ;;
            *)
                echo -n "Please anwser 'y' or 'n'. "
                ;;
        esac
    done
}


function expandPath {
    if [[ "$1" =~ ^~/ ]]; then
        echo "$HOME/${1#"~/"}"
    elif [[ ! "$1" =~ ^/ ]]; then
        echo "$HOME/$1"
    else
        echo "$1"
    fi
}


function listTargets {
    echo "Available targets:"
    echo "=================="
    awk "$AWK_LIST_TARGETS" "$CONF" | sort
    exit
}


function parseTarget {
    awk "$AWK_PARSE_TARGET" OFS='\t' TARGET="$1" "$CONF"
}


AWK_COMMON='
BEGIN {
    header_left  = "^\\s*\\["
    header_right = "\\]\\s*$"
    header_tail = "\\.(links|hooks|installs|needs)"
    valid_target = "[a-zA-Z0-9\\-_]+"
}'


AWK_LIST_TARGETS='
'"$AWK_COMMON"'

$0 ~ header_left valid_target header_tail header_right {
    target = $0
    sub(header_left, "", target)
    sub(header_right, "", target)
    sub(header_tail, "", target)
    targets[target]
}

END { for (target in targets) { print target } }'


AWK_PARSE_TARGET='
'"$AWK_COMMON"'

BEGIN { comment_or_empty = "^\\s*(;|\\s*$)" }

$0 ~ header_left valid_target header_tail header_right {
    header = parse_header($0)
    in_target = header ~ TARGET header_tail
    TYPE = parse_type(header)
}

in_target && $0 !~ header_left && $0 !~ comment_or_empty {
    print TYPE, parse_key($0), parse_value($0)
}

function trim(s) {
    sub(/^\s*/, "", s)
    sub(/\s*$/, "", s)
    return s
}

function parse_key(line) {
    sub(/=.*$/, "", line)
    return trim(line)
}

function parse_value(line) {
    sub(/^[^=]*=/, "", line)
    return trim(line)
}

function parse_type(header) {
    sub(valid_target "\\.", "", header)
    return header
}

function parse_header(line) {
    sub(header_left,  "", line)
    sub(header_right, "", line)
    return line
}'


function cwrap {
    echo -ne "$1$2\e[0m"
}


function cpipe {
    echo -ne "$1"
    cat
    echo -ne "\e[0m"
}


function clog {
    echo -n "$(cwrap "$1" "$2: ")"
    shift; shift
    echo -e "$@"
}


function error {
    clog "$ERROR" "Error" "$@"
}


function warning {
    clog "$WARNING" "Warning" "$@"
}


function info {
    clog "$INFO" "Info" "$@"
}


main "$@"
