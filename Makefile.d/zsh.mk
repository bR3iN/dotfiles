ZDOTDIR := $(or $(ZDOTDIR),$(CONFIG_DIR)/zsh)
ZSH_PLUGINS = $(ZDOTDIR)/plugins
ZSH_DATA_DIR = $(or $(XDG_DATA_HOME),$(HOME)/.local/share)/zsh

.PHONY: zsh
zsh: \
	$(HOME)/.zshenv \
	$(CONFIG_DIR)/zsh/.zshrc \
	$(CONFIG_DIR)/zsh/zshrc.d \
	$(ZSH_PLUGINS)/zsh-syntax-highlighting \
	$(ZSH_PLUGINS)/zsh-autosuggestions \
	$(ZSH_PLUGINS)/zsh-autocomplete \
	$(ZSH_DATA_DIR)

$(ZSH_DATA_DIR):
	mkdir -p "$@"

$(ZSH_PLUGINS)/zsh-syntax-highlighting: URL = https://github.com/zsh-users/zsh-syntax-highlighting
$(ZSH_PLUGINS)/zsh-autosuggestions: URL = https://github.com/zsh-users/zsh-autosuggestions
$(ZSH_PLUGINS)/zsh-autocomplete: URL = https://github.com/marlonrichert/zsh-autocomplete

$(ZSH_PLUGINS)/%: | $(ZSH_PLUGINS)
	git clone "$(URL)" "$@"

$(ZSH_PLUGINS):
	mkdir -p "$(ZSH_PLUGINS)"
