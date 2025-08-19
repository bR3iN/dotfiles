#!/usr/bin/bash

which cargo &> /dev/null && exit

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
    --output /tmp/rustup-init.sh

chmod +x /tmp/rustup-init.sh
/tmp/rustup-init.sh -y \
    --default-host x86_64-unknown-linux-gnu \
    --default-toolchain stable \
    --profile default \
    --no-modify-path
rm /tmp/rustup-init.sh
