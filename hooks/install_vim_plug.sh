#!/usr/bin/bash
#
# https://github.com/junegunn/vim-plug

INSTALL_PATH="${HOME}/.vim/autoload/plug.vim"

if [ ! -e "${INSTALL_PATH}" ]; then 
	curl -fLo "${INSTALL_PATH}" --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi
