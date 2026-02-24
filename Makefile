CONFIG_DIR ?= $(or $(XDG_CONFIG_HOME),$(HOME)/.config)
USER_THEMES ?= $(HOME)/.themes
SYSTEM_THEMES ?= /usr/share/themes

# Have to come before includes to be used
STAMP = .tag
OS_ID ?= $(shell source /etc/os-release && echo $$ID)
FLATPAK_APP ?= /var/lib/flatpak/app

###########
# Targets #
###########

.PHONY: autorandr
autorandr: $(CONFIG_DIR)/autorandr

.PHONY: bash
bash: \
	$(HOME)/.bashrc \
	$(HOME)/.bash_profile

.PHONY: bat
bat: $(CONFIG_DIR)/bat/config

.PHONY: cmus
cmus: $(CONFIG_DIR)/cmus/rc

.PHONY: fish
fish: $(CONFIG_DIR)/fish
	sudo mkdir -p /root/.config
	# sudo cp -r config/fish /root/.config
	xdg-mime default thunar.desktop inode/directory

.PHONY: git
git: $(CONFIG_DIR)/git/config

.PHONY: helix
helix: $(CONFIG_DIR)/helix

.PHONY: i3
i3: \
	$(CONFIG_DIR)/i3 \
	$(CONFIG_DIR)/polybar \
	$(HOME)/.xinitrc \
	$(CONFIG_DIR)/dunst

.PHONY: kitty
kitty: $(CONFIG_DIR)/kitty
	touch $(CONFIG_DIR)/kitty/local.conf

.PHONY: npm
npm: $(STAMP)/npm
$(STAMP)/npm:
	@mkdir -p $(HOME)/.node_modules
	npm config set prefix $(HOME)/.node_modules
	@touch "$@"

.PHONY: nvim
nvim: $(CONFIG_DIR)/nvim

.PHONY: nvim-docsets
nvim-docsets:
	curl -o "$$HOME/.local/share/fennel-ls/docsets/nvim.lua" "https://git.sr.ht/~micampe/fennel-ls-nvim-docs/blob/main/nvim.lua"

.PHONY: profile
profile: $(HOME)/.profile $(HOME)/.profile.d

$(HOME)/.profile.d:
	mkdir -p "$@"

.PHONY: qutebrowser
qutebrowser: \
	$(CONFIG_DIR)/qutebrowser/config.py \
	$(CONFIG_DIR)/qutebrowser/base16.py

.PHONY: redshift
redshift: $(CONFIG_DIR)/redshift.conf

.PHONY: rofi
rofi: $(CONFIG_DIR)/rofi

.PHONY: rofi-scripts
rofi-scripts: $(HOME)/.local/libexec/prompt.sh

.PHONY: starship
starship: \
	$(CONFIG_DIR)/starship.toml \
	$(CONFIG_DIR)/starship-post.toml
	# cargo install --locked starship

.PHONY: tldr
tldr: $(CONFIG_DIR)/tealdeer

.PHONY: tmux
tmux: $(CONFIG_DIR)/tmux

.PHONY: wofi
wofi: \
	$(CONFIG_DIR)/wofi \
	$(HOME)/.local/bin/wofi-emoji

$(HOME)/.local/bin/wofi-emoji:
	curl --create-dirs -o $@ https://raw.githubusercontent.com/Zeioth/wofi-emoji/master/wofi-emoji
	chmod +x $@

.PHONY: zathura
zathura: $(CONFIG_DIR)/zathura
	xdg-mime default org.pwmt.zathura.desktop application/pdf
	xdg-mime default org.pwmt.zathura.desktop application/epub+zip
	xdg-mime default org.pwmt.zathura.desktop image/vnd.djvu

.PHONY: zellij
zellij: $(CONFIG_DIR)/zellij


$(FLATPAK_APP)/%: | /usr/bin/flatpak 
	sudo flatpak install "$*"

/usr/bin/flatpak:
ifneq ($(filter $(OS_ID),opensuse-tumbleweed opensuse-leap),)
	sudo zypper install -y flatpak flatpak-remote-flathub
else ifneq ($(filter $(OS_ID),debian ubuntu),)
	sudo apt install -y flatpak
else
	@echo "Can't autoinstall flatpak for os '$(OS_ID)'"
	@false
endif

include Makefile.d/*


#########
# Utils #
#########

$(CONFIG_DIR)/%: config/%
	$(call link,$<,$@)

$(HOME)/.local/libexec/%: lib/%
	$(call link,$<,$@)

$(HOME)/.%: home/%
	$(call link,$<,$@)

download = curl -fLo "$(2)" "$(1)"
copy = mkdir -p "$(dir $(2))" && cp -rTi "$(1)" "$(2)"
define link
	@mkdir -p "$(dir $(2))"
	@# Remove broken existing symlinks without asking
	@if [ -L "$(2)" ] && ! [ -e "$(2)" ]; then rm "$(2)"; fi
	ln -sTi "$(abspath $(1))" "$(2)"
endef

/usr/local/bin/%: system/bin/%
	@# NOTE: `-D` creates missing directories
	sudo install -D -m755 "$<" "$@"

# FIXME: Double check that this _can't_ be in `/usr/local` instead
/usr/share/wayland-sessions/%: system/wayland-sessions/%
	sudo sh -c "$(call copy,$<,$@)"

reset:
	rm -f $(STAMP)/*
