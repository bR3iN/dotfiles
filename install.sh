INSTALLER_DIR=$(dirname $(readlink -f "$0"))
CONF="${INSTALLER_DIR}/test.ini"
declare -A INSTALLED


function main {

	if [ "$1" = "list" ]; then
		listTargets
	fi

	for target in "$@"; do 
		echo
		echo
		echo "target: '${target}'"
		if [ ! ${INSTALLED["$target"]} ];then
			installTarget
			INSTALLED+=(["$target"]=true)
		else
			echo "target already installed"
		fi
	done
}


function listTargets {
	echo "Available targets:"
	echo "=================="
	awk -f "${INSTALLER_DIR}/getsections.awk" "$CONF"
	exit
}


function installTarget { 

	keys=$(awk -F= -v TARGET_SECTION="$target" -f \
		"getkeys.awk" "$CONF")

	IFS=$'\n' 
	for key in $keys; do
		if [ ! -z "$key" ]; then
			IFS=':' read -ra _key <<< "$key"
			name="${_key[0]}"
			value="${_key[1]}"

			echo "'$name'='$value'"
		fi

		if [ "$name" = "cmd" ]; then
			eval "$value"
		elif [ "$name" = "hook" ];then
			("${INSTALLER_DIR}/hooks/${value}")
		else
			createSymlink
		fi

	done

}


function createSymlink {

	getPath

	if [ -e "$path" ]; then
		if [ ! -h "$path" ]; then
			confirmAndDelete 
		else
			rm -rf "$path"
		fi
	fi

	if [ "$?" -eq 0 ]; then

		mkdir -p "$(dirname "$path")"
		file="${INSTALLER_DIR}/${name}"
		echo "symlink '$file' to '$path'"
		[ -e "$file" ] && ln -s "$file" "$path"

	fi
	}


function confirmAndDelete {
	echo
	echo "The file/directory"
	echo
	echo "		'$path'"
	echo
	echo -n "already exists. Do you want to delete it? [y|N] "

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
	elif [[ ! "$path" =~ ^/ ]]; then
		path="${HOME}/${value}"
	else
		path="$value"
	fi

}


main "$@"
