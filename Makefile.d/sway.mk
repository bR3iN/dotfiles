.PHONY: sway
sway: \
	kanshi \
	sway-session \
	$(CONFIG_DIR)/sway \
	$(CONFIG_DIR)/waybar \
	$(CONFIG_DIR)/mako \
	config/sway/config.d/config.local

config/sway/config.d/config.local: config/sway/config-template
	cp -n "$<" "$@"

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
