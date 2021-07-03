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
    playerctl
    rofi-wayland
    sway
    swayidle
    Thunar
    greetd
    greetd-gtkgreet
    thunar-volman
    tumbler
    swaylock
    waybar
    wl-clipboard
    wlogout
    wlsunset
)

sudo dnf install -y "${pkgs[@]}"
sudo dnf group install -y "${groups[@]}"

sudo systemctl enable greetd
