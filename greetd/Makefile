.PHONY: install

install:
	install -m0644 -D -t /etc/greetd config.toml
	install -m0644 -D -t /etc/greetd gtkgreet.css
	install -m0644 -D -t /etc/greetd gtkgreet-sway.conf
	systemctl set-default graphical.target
