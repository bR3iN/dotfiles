sudo dnf module enable sway:rolling
sudo dnf copr   enable alebastr/sway-extras

pkgs=(
    blueberry
    brightnessctl
    d-feet
    google-noto-emoji-color-fonts
    gnome-keyring
    seahorse
    htop
    lxappearance
    NetworkManager-tui
    nnn
    pavucontrol
    picom
    playerctl
    rofi-wayland
    sway
    swayidle
    Thunar
    thunar-volman
    torbrowser-launcher
    tumbler
    wl-clipboard
    wlsunset
)

sudo dnf install -y "${pkgs[@]}"
sudo dnf group install -y "${groups[@]}"
