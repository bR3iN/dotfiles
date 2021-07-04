sudo dnf module enable -y sway:rolling
sudo dnf copr   enable -y alebastr/sway-extras

pkgs=(
    blueberry
    brightnessctl
    d-feet
    gnome-keyring
    google-noto-emoji-color-fonts
    greetd
    greetd-gtkgreet
    htop
    lxappearance
    NetworkManager-tui
    nnn
    pavucontrol
    playerctl
    pulseaudio-utils
    rofi-wayland
    seahorse
    sway
    swayidle
    swaylock
    Thunar
    thunar-volman
    tumbler
    waybar
    wl-clipboard
    wlogout
    wlsunset
    wob
    xfce-polkit
)

sudo dnf install -y "${pkgs[@]}"
sudo dnf group install -y "${groups[@]}"

sudo systemctl enable greetd
