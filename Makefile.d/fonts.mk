FONT_DIR := /usr/local/share/fonts
NERD_FONTS_VERSION := v3.3.0
NERD_FONTS_URL := https://github.com/ryanoasis/nerd-fonts/releases/download/$(NERD_FONTS_VERSION)

.PHONY: fonts
fonts: \
	$(FONT_DIR)/FiraCodeNerdFont-Retina.ttf \
	$(FONT_DIR)/NotoSansNerdFont-Regular.ttf

$(FONT_DIR)/FiraCodeNerdFont-Retina.ttf: | $(FONT_DIR)
	$(call install-nerd-font,FiraCode)

$(FONT_DIR)/NotoSansNerdFont-Regular.ttf: | $(FONT_DIR)
	$(call install-nerd-font,FiraCode)

$(FONT_DIR):
	sudo mkdir -p "$(FONT_DIR)"

define install-nerd-font
	@echo "Installing $(1) Nerd Font"
	$(call download,$(NERD_FONTS_URL)/$(1).zip,/tmp/$(1)-nf.zip)
	sudo unzip -qd "$(FONT_DIR)" /tmp/$(1)-nf.zip '*.ttf'
	rm "/tmp/$(1)-nf.zip"
endef
