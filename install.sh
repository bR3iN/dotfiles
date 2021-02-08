#!/bin/bash

set -e
#DOT_DIR=$(dirname $(readlink -f "$0"))
DOT_DIR="$HOME/.dotfiles"
LOG_FILE="${DOT_DIR}/.log"
DEBUG=true

# Set targets
TARGETS=(\
	alacritty:.alacritty.yml
)

function main {
		for target in ${TARGETS[@]}; do
			InstallTarget $target
		done
	}	

function InstallTarget {

	local target
	IFS=':' read -ra target <<< "$1" 
	file=${target[0]}
	path=${target[1]}

	ParsePath "$path"

	if [ -e "$file" ]; then
		Symlink "$file" "$path"
	fi
}


function Symlink {

	local file="$1"
	local path="$2"

	if [ -e "$path" ] && [ ! -h "$path" ]; then

		ConfirmAndDelete "$path"
		# Returns non-zero exit code if $path is not deleted

	elif [ -h "$path" ]; then

		rm -rf "$path" && Log "Removed symbolic link at ${path}."
		# Current exit code is the one from rm -rf $path

	fi

	if [ "$?" = 0 ]; then

		mkdir -p $(dirname "$path")
		ln -s ${DOT_DIR}/$file $path
		Log "Symlinked $path to $file" 

	fi
}

function ParsePath {

	if [[ "$path" =~ ^~/ ]]; then
		path="${HOME}/${path#"~/"}"
	elif [[ ! "$path" =~ ^/ ]]; then
		path="${HOME}/${path}"
	fi

}

function ConfirmAndDelete {

	local path="$1"

	echo "The file ${path} alread exists. Do you want to delete it [yN]?"

	while true; do

		read confirm
		
		case "$confirm" in
			y|Y) rm -rf "$path" && Log "Removed $1" && return 0
				return 1;;
			n|N) return 1;;
		esac
		echo "Please anwser 'y' or 'n'."
	done
	
}

function Log {
	echo "[$(date +"%F %T")] $@" >> $LOG_FILE
	echo "$@"
}

main
