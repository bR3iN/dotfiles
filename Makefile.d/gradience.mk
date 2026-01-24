ADW_GTK3_URL = https://github.com/lassekongo83/adw-gtk3/releases/download/v4.5/adw-gtk3v4-5.tar.xz

GRADIENCE_DIR = $(HOME)/.var/app/com.github.GradienceTeam.Gradience
GRADIENCE_USER_PRESETS ?= $(GRADIENCE_DIR)/config/presets/user/

GRADIENCE_ID = com.github.GradienceTeam.Gradience

.PHONY: gradience
gradience: $(STAMP)/gradience

$(STAMP)/gradience: \
		$(FLATPAK_APP)/$(GRADIENCE_ID) \
		$(USER_THEMES)/adw-gtk3 \
		$(HOME)/.local/libexec/propagate-theme.sh
	# Generic gradience setup
	flatpak run --command=gradience-cli $(GRADIENCE_ID) flatpak-overrides -e both

	# Install base16 color theme
	$(call link,lib/gradience/Base16.json,$(GRADIENCE_USER_PRESETS)/Base16.json)
	flatpak run --command=gradience-cli $(GRADIENCE_ID) apply --gtk both -n Base16

	mkdir -p $(CONFIG_DIR)/gtk-{4,3}.0
	# Set user theme
	gsettings set org.gnome.desktop.interface gtk-theme 'adw-gtk3'
	# Set flatpak theme
	sudo flatpak override --env=GTK_THEME=adw-gtk3

	# Let flatpak apps access themes, gtk config and symlinks to gradience/Base16.json
	sudo flatpak override --filesystem="$(PWD)/lib/gradience"
	sudo flatpak override --filesystem=home/.themes
	sudo flatpak override --filesystem=xdg-config/gtk-4.0
	sudo flatpak override --filesystem=xdg-config/gtk-3.0

	@touch "$@"


# adw-gtk3 theme
.PHONY: adw-gtk3
adw-gtk3: \
	$(USER_THEMES)/adw-gtk3 \
	$(USER_THEMES)/adw-gtk3-dark

$(USER_THEMES)/adw-gtk3 $(USER_THEMES)/adw-gtk3-dark &: \
		$(USER_THEMES)/%: $(SYSTEM_THEMES)/%
	cp -iTr "$<" "$@"

$(SYSTEM_THEMES)/adw-gtk3 $(SYSTEM_THEMES)/adw-gtk3-dark &:
	$(call download,$(ADW_GTK3_URL),/tmp/adw-gtk3.tar.xz)
	sudo mkdir -p "$(SYSTEM_THEMES)"
	sudo tar xaf /tmp/adw-gtk3.tar.xz --directory="$(SYSTEM_THEMES)"
	rm /tmp/adw-gtk3.tar.xz
