#!/usr/bin/bash

# Install flavours and update templates
cargo install flavours
~/.cargo/bin/flavours update all

# Make sure all necessary directories exist
mkdir -p ~/.config/{gtk-2.0,gtk-3.0,gtk-3.20,waybar,sway/config.d,kitty,zathura,dunst,mako}

# Apply default theme
~/.cargo/bin/flavours apply --light pop-os
