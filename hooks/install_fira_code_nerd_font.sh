FONT_PATH="$HOME/.local/share/fonts"
TMP="$FONT_PATH/tmp.zip"
URL='https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip'

if ! ls "$FONT_PATH" | egrep -q 'Fira Code.*Nerd Font'; then
    mkdir -p "$FONT_PATH"
    echo "Downloading Fira Code Nerd Font to $HOME/.local/share/fonts..."
    wget "$URL" -O "$TMP" &>> /dev/null
    echo "Extracting font..."
    unzip -d "$FONT_PATH" "$TMP" &>> /dev/null
    rm "$TMP"
fi
