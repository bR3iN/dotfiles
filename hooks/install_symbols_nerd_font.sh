FONT_PATH="$HOME/.local/share/fonts"

FONT_NAME="Symbols Nerd Font"
URL='https://github.com/ryanoasis/nerd-fonts/raw/master/src/glyphs/Symbols-1000-em%20Nerd%20Font%20Complete.ttf'

if ! ls "$FONT_PATH" | egrep -q -i 'Symbols.*Nerd.*Font'; then
    mkdir -p "$FONT_PATH"
    echo "Downloading $FONT_NAME to $HOME/.local/share/fonts..."
    wget "$URL" -P "$FONT_PATH"
fi
