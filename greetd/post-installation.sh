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
        # Fix "PAM 'greetd' service missing"
        ln -s /usr/etc/pam.d/greetd /etc/pam.d/greetd

        sed 's/user = "greetd"/user = "greeter"/' -i "$GREETD_CONF"
        chown greeter:greeter "$GREETD_CONF"

        # Fix greeter's home directory
        mkdir -p /var/lib/greetd/.config/dconf
        chown -R greeter:greeter /var/lib/greetd
        ;;
esac
