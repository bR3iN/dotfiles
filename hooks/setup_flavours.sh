#!/usr/bin/bash

# Install flavours and update templates
cargo install flavours
~/.cargo/bin/flavours update all

# Make sure all necessary directories exist
mkdir -p ~/.config/{gtk-2.0,gtk-3.0,gtk-3.20,waybar,sway,kitty,alacritty,zathura,mako}

# Make rofi use the flavours infused theme
mkdir -p ~/.profile.d
echo 'export ROFI_FLAGS="$ROFI_FLAGS -theme theme"' > ~/.profile.d/rofi_flavours

# Apply default theme
~/.cargo/bin/flavours apply --light pop-os
