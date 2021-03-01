#!/bin/bash
set -e

#This file's directory.
INSTALLER_DIR=$(dirname $(readlink -f "$0")) 

#Path to config file.
CONF="${INSTALLER_DIR}/dotfiles.ini" 

#Seperator used internally. Is not allowed in keys.
OUTPUT_SEPERATOR=":" 

function main {

	if [ "$1" = "list" ]; then
		listTargets
	fi

	declare -A INSTALLED

	for target in "$@"; do 
			installTarget "$target"
	done

}


function listTargets {
	echo "Available targets:"
	echo "=================="
	awk -f - "$CONF" << EOF
	/^\[.*\]/ { 
	gsub(/(\[( |	)*|( |	)*\])/,"")
	print
}
EOF
exit
}


function installTarget { 

	local target="$1"

	if [ ${INSTALLED["$target"]} ]; then
		return 0
	else
		INSTALLED+=(["$target"]=true)
	fi

	#local keys=$(awk -F= -v TARGET_SECTION="$target" -f \
		#"getkeys.awk" "$CONF")
	local keys=$(getKeys)

	if [ -z "$keys" ]; then
		echo "Target '$target' was not found."
		return 1
	fi

	local IFS=$'\n' 
	for key in $keys; do

		#split key into name and value
        if [ "$key" != "$OUTPUT_SEPERATOR" ]; then
            IFS="$OUTPUT_SEPERATOR" read -ra _key <<< "$key"
            name="${_key[0]}"
            value="${_key[1]}"

            if [ "$name" = "cmd" ]; then
                eval "$value"

            elif [ "$name" = "hook" ]; then
                ("${INSTALLER_DIR}/hooks/${value}")

            elif [ "$name" = "target" ]; then
                installTarget "$value"

            elif [ ! -z "$value" ]; then
                createSymlink

            fi
        fi

	done

	echo "Installed '$target'"

}


function createSymlink {

	getPath

	if [ ! -e "${INSTALLER_DIR}/${name}" ]; then
		echo "WARNING: Link destination '${INSTALLER_DIR}/${name}' was not found."
	fi

	if [ -e "$path" ]; then
		if [ ! -h "$path" ]; then
			confirmAndDelete 
		else
			rm -rf "$path"
		fi
	fi

	if [ "$?" -eq 0 ]; then

        [ -h "$path" ] && rm -rf "$path"
		mkdir -p "$(dirname "$path")"

		file="${INSTALLER_DIR}/${name}"
		[ -e "$file" ] && ln -s "$file" "$path"

		if [ "$?" -eq 0 ]; then
			echo "Created the symbolic link"
			echo "$file --> $path"
		fi

	fi
}


function confirmAndDelete {
	echo
	echo "The file/directory"
	echo
	echo "		'$path'"
	echo
	echo -n "already exists. Do you want to delete it? [yN] "

	while true; do

		read confirm

		case "$confirm" in
			y|Y) rm -rf "$path" && return 0
				return 1
				;;
			n|N) 
				echo "not deleted"
				return 1
				;;
			"") 
				echo "not deleted"
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
	mawk -F= -v TARGET_SECTION="$target" \
		-v OUTPUT_SEPERATOR="$OUTPUT_SEPERATOR" \
		-f - "$CONF" << EOF
			BEGIN {
			in_target_section = 0 
			ws = "( |	)*"
		}

# Matches non-target header
/^\[.*\]/ \
    && \$0 !~ "^\\\\[" ws TARGET_SECTION ws "\\\\]" \
    {
        in_target_section = 0 
    }

in_target_section \
	&& \$0 !~ "^" ws ";" \
	&& \$0 ~ ws "[^ 	]" \
	{
		gsub(/(^(	| )+|(	| )+\$)/,"",\$1)
		gsub(/(^(	| )+|(	| )+\$)/,"",\$2)
		print \$1 OUTPUT_SEPERATOR \$2
	}

# Matches target header
\$0 ~ "^\\\\[" ws TARGET_SECTION ws"\\\\]" \
	{
		in_target_section = 1 
	}
EOF
}


main "$@"
