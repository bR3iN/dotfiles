sudo zypper addrepo -refresh -name "X11:Wayland" \
    "https://download.opensuse.org/repositories/X11:/Wayland/openSUSE_Tumbleweed/X11:Wayland.repo"

pkgs=(
    bat
    blueberry
    cargo
    docker
    firefox
    fish
    gnome-keyring
    greetd
    gtkgreet
    kitty
    mpv
    mpv-mpris
    neofetch
    neovim
    npm
    opi
    pipewire
    pipewire-pulseaudio
    qutebrowser
    rofi
    seahorse
    secret-tool
    sway
    tmux
    upower
    wlock
    wob
    zathura
    zathura-plugin-djvu
    zathura-plugin-ps

    # Required by packages above
    libqt5-qtwayland # Needed by qutebrowser on wayland
    libstdc++-devel  # Needed by treesitter(?)
)

systemctl enable --user --now pipewire
systemctl enable --user --now pipewire-pulseaudio
sudo systemctl enable --now docker
sudo systemctl enable       greetd

sudo zypper install -y "$pkgs[@]"
