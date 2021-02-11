#!/usr/bin/bash
#
# from https://github.com/Kurgol/keychron/blob/master/k2.md

if [ ! -z "$(grep "options hid_apple fnmode=2" /etc/modprobe.d/hid_apple.conf 2> /dev/null)" ]; then
	exit
fi

if [ $UID -ne 0 ]; then
	echo "Please run as root."
	exit 
fi

echo "options hid_apple fnmode=2" >> /etc/modprobe.d/hid_apple.conf

update-initramfs -u
rmmod hid_apple 
modprobe hid_apple.
