WALLPAPER ?= $(HOME)/Wallpaper

.PHONY: wallpaper
wallpaper: \
	$(WALLPAPER)/Pop-OS.jpg \
	$(WALLPAPER)/Tokyonight.jpg \
	$(WALLPAPER)/Catpuccin.png \
	$(WALLPAPER)/Gruvbox.png

$(WALLPAPER):
	mkdir -p "$@"

$(WALLPAPER)/Pop-OS.jpg: URL = https://raw.githubusercontent.com/pop-os/wallpapers/master/original/nasa-89125.jpg
$(WALLPAPER)/Pop-OS.jpg: CONVERT = -resize 3840 -crop 3840x2160+0+0 -gravity center

$(WALLPAPER)/Tokyonight.jpg: URL = https://getwallpapers.com/wallpaper/full/c/3/4/49335.jpg

$(WALLPAPER)/Catpuccin.png: URL = https://preview.redd.it/catpuccin-wallpapers-6094x2344-3440x1440-v0-ycpfbe13hxu91.png?width=6094&format=png&auto=webp&s=5fa2a8980b1124ae05b9fa4e4b7c53a37f2ac505
$(WALLPAPER)/Catpuccin.png: CONVERT = -resize 3840 -crop 3840x2160+0+0 -gravity center

$(WALLPAPER)/Gruvbox.webp: URL = https://preview.redd.it/gruvbox-abstract-shapes-7680x4320-v0-uhig907t7dca1.png?auto=webp&s=c7ebd03163080183a7e498e2c3f3f3bdb3aaebb3
$(WALLPAPER)/Gruvbox.webp: CONVERT = -resize 3840 -crop 3840x2160+0+0 -gravity center

$(WALLPAPER)/Gruvbox.png: $(WALLPAPER)/Gruvbox.webp
	magick "$<" "$@"

$(WALLPAPER)/Pop-OS.png: $(WALLPAPER)/Pop-OS.jpg
	magick "$<" "$@"

$(WALLPAPER)/%: | $(WALLPAPER)
	$(call download,$(URL),$@)
	$(if $(CONVERT),convert $(CONVERT) "$@" "$@")
