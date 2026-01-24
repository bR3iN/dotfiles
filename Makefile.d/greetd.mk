.PHONY: greetd
greetd: $(STAMP)/greetd

$(STAMP)/greetd: \
	/etc/greetd/config.toml \
	/etc/pam.d/greetd
	# /etc/greetd/gtkgreet-sway.conf
	# /etc/greetd/gtkgreet.css
	systemctl enable greetd.service
	systemctl set-default graphical.target
