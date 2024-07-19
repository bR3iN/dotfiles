.PHONY: install

install:
	mkdir -p ~/.config/systemd/user
	cp *.service ~/.config/systemd/user/
	cp ./tablet-mode.conf ~/.config/sway/config.d/
	# sed "s#{EWW}#$$(which eww)#g" toolbar.service.in > ~/.config/systemd/user/toolbar.service
