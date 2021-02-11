#!/usr/bin/bash
#
# https://github.com/junegunn/vim-plug

INSTALL_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/nvim/site/autoload/plug.vim"

if [ ! -e "${INSTALL_PATH}" ]; then 
	curl -fLo "${INSTALL_PATH}" --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi
