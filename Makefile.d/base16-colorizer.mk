BASE16_COLORIZER_VERSION ?= 0.1.0

BASE16_COLORIZER_URL = https://github.com/bR3iN/base16-colorizer/releases/download/v0.1.0/base16-colorizer-v$(BASE16_COLORIZER_VERSION)-x86_64-linux

.PHONY: base16-colorizer
base16-colorizer: \
	$(CONFIG_DIR)/base16-colorizer \
	$(HOME)/.local/bin/base16-colorizer \
	$(STAMP)/ignore-base16

$(STAMP)/ignore-base16:
	scripts/ignore-base16.sh on
	@touch "$@"

$(HOME)/.local/bin/base16-colorizer:
	$(call download,$(BASE16_COLORIZER_URL),$@)
	chmod +x "$@"
