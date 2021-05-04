if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ] \
    || [ -n "$SSH_CONNECTION" ]; then
    exit 0
else
    exit 1
fi
