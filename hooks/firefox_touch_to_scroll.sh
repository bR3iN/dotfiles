LINE="MOZ_USE_XINPUT2 DEFAULT=1"
CONF="/etc/security/pam_env.conf"

if ! grep -q "$LINE" "$CONF"; then
    echo "Appending to ${CONF}, please enter password if necessary."
    echo "$LINE" | sudo tee -a "$CONF" >> /dev/null
fi
