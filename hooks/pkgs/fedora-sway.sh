sudo dnf module enable sway:rolling
sudo dnf copr   enable alebastr/sway-extras

pkgs=(
    blueberry
    brightnessctl
    d-feet
    google-noto-emoji-color-fonts
    htop
    lxappearance
    NetworkManager-tui
    nnn
    pavucontrol
    picom
    playerctl
    rofi-wayland
    sway
    Thunar
    thunar-volman
    torbrowser-launcher
    tumbler
    wl-clipboard
)

sudo dnf install -y "${pkgs[@]}"
sudo dnf group install -y "${groups[@]}"
