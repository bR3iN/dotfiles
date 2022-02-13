#!/usr/bin/bash

# Install flavours and update templates
~/.cargo/bin/cargo install --locked flavours
~/.cargo/bin/flavours update all

# Apply default theme
~/.cargo/bin/flavours apply --light pop-os
