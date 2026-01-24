#!/usr/bin/bash
#
# from https://github.com/Kurgol/keychron/blob/master/k2.md

if ! grep -q "options hid_apple fnmode=2" /etc/modprobe.d/hid_apple.conf &> /dev/null; then
    echo "options hid_apple fnmode=2" | sudo tee -a /etc/modprobe.d/hid_apple.conf
    sudo update-initramfs -u
    sudo rmmod hid_apple && sudo modprobe hid_apple.
fi
