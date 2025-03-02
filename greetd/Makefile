.PHONY: install

install:
	install -m0644 -D -t /etc/greetd config.toml
	install -m0644 -D -t /etc/greetd gtkgreet.css
	install -m0644 -D -t /etc/greetd gtkgreet-sway.conf
	install -m0644 -D -T pam_greetd /etc/pam.d/greetd
	systemctl enable greetd.service
	systemctl set-default graphical.target
