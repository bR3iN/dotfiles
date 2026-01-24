SWAY_SCRATCHPAD_URL := https://github.com/bR3iN/sway-scratchpad-waybar/releases/download/v0.1.0/sway-scratchpad-waybar-0.1.0-x86_64-linux-gnu

.PHONY: sway
sway: \
	kanshi \
	sway-session \
	$(CONFIG_DIR)/sway \
	$(CONFIG_DIR)/waybar \
	$(CONFIG_DIR)/mako \
	config/sway/config.d/config.local \
	$(HOME)/.local/bin/sway-scratchpad-waybar

config/sway/config.d/config.local: config/sway/config-template
	cp -n "$<" "$@"

$(HOME)/.local/bin/sway-scratchpad-waybar:
	$(call download,$(SWAY_SCRATCHPAD_URL),$@)
	chmod +x "$@"

.PHONY: sway-session
sway-session: \
	/usr/local/bin/mysway \
	/usr/share/wayland-sessions/mysway.desktop


.PHONY: kanshi
kanshi: $(CONFIG_DIR)/kanshi config/kanshi/config

config/kanshi/config: config/kanshi/config-template
	cp -n "$<" "$@"


# /usr/local/bin/mysway: Sway/session/mysway
# 	sudo install -m755 $< $@
