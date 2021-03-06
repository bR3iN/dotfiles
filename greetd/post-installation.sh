#!/usr/bin/bash
GREETD_CONF=/etc/greetd/config.toml

if [ "$USER" != root ]; then
    echo "Please run as root"
    exit
fi

if [ -z "$INSTALLER_USER" ]; then
    echo "WARNING: environment variable 'INSTALLER_USER' not found"
    exit
fi

sed 's/%USER%/'"$INSTALLER_USER"'/g' -i "$GREETD_CONF"

# Distro specific configuration
case "$(cat /etc/os-release | grep '^NAME=')" in
    *Tumbleweed*)
        sed 's/user = "greetd"/user = "greeter"/' -i "$GREETD_CONF"
        chown greeter:greeter "$GREETD_CONF"
        ;;
esac
