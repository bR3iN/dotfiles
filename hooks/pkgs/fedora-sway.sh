sudo dnf module enable -y sway:rolling
sudo dnf copr   enable -y alebastr/sway-extras

pkgs=(
    blueberry
    brightnessctl
    d-feet
    google-noto-emoji-color-fonts
    gnome-keyring
    xfce-polkit
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
    wob
    wlsunset
)

sudo dnf install -y "${pkgs[@]}"
sudo dnf group install -y "${groups[@]}"

sudo systemctl enable greetd
