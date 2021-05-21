#!/usr/bin/bash
set -e

URL="https://raw.githubusercontent.com/pop-os/wallpapers/master/original/nasa-89125.jpg"
WALLPAPER_DIR="/usr/share/backgrounds"
EXTENSION="${URL##*.}"
TARGET_SIZE="1920x1080"
X_OFFSET=0
Y_OFFSET=372

TEMP_DIR="$(mktemp -d)"

echo "Download wallpaper..."
wget -O "$TEMP_DIR/original.$EXTENSION" "$URL" &>> /dev/null

echo "Convert wallpaper to PNG, crop wallpaper and blur wallpaper for login and lock screen..."
convert -resize "${TARGET_SIZE%%x*}" "$TEMP_DIR/original.$EXTENSION" "$TEMP_DIR/mywallpaper.png"
convert -crop "${TARGET_SIZE}+${X_OFFSET}+${Y_OFFSET}" "$TEMP_DIR/mywallpaper.png" "$TEMP_DIR/mywallpaper.png"
convert -blur 0x30 "$TEMP_DIR/mywallpaper.png" "$TEMP_DIR/mywallpaper-blurred.png"

echo "Installing wallpaper..."
sudo install -m0644 -D --target-directory="$WALLPAPER_DIR" "$TEMP_DIR/mywallpaper.png"
sudo install -m0644 -D --target-directory="$WALLPAPER_DIR" "$TEMP_DIR/mywallpaper-blurred.png"

rm -r "${TEMP_DIR:?}"
