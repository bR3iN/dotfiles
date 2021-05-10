FONT_PATH="$HOME/.local/share/fonts"
TMP="$FONT_PATH/tmp.zip"

FONT_NAME="Fira Code Nerd Fonts"
URL='https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/FiraCode.zip'

if ! ls "$FONT_PATH" | egrep -q 'Fira.*Code.*Nerd Font'; then
    mkdir -p "$FONT_PATH"
    echo "Downloading $FONT_NAME to $HOME/.local/share/fonts..."
    wget "$URL" -O "$FONT_PATH/tmp.zip" &>> /dev/null
    echo "Extracting fonts..."
    unzip -d "$FONT_PATH" "$TMP" &>> /dev/null
    rm "$TMP"
fi
