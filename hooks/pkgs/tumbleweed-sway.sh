sudo zypper addrepo -refresh -name "X11:Wayland" \
    "https://download.opensuse.org/repositories/X11:/Wayland/openSUSE_Tumbleweed/X11:Wayland.repo"

pkgs=(
    bat
    blueberry
    cargo
    d-feet
    docker
    firefox
    fish
    gnome-keyring
    greetd
    gtkgreet
    kitty
    libnotify-tools
    mako
    mpv
    mpv-mpris
    neofetch
    neovim
    npm
    opi
    pipewire
    pipewire-pulseaudio
    qutebrowser
    seahorse
    secret-tool
    sway
    thunar
    tmux
    upower
    waybar
    wlock
    wob
    zathura
    zathura-plugin-djvu
    zathura-plugin-ps

    # Required by packages above
    libqt5-qtwayland # Needed by qutebrowser on wayland
    libstdc++-devel  # Needed by treesitter(?)
)

sudo zypper install -y "$pkgs[@]"

systemctl enable --user --now pipewire
systemctl enable --user --now pipewire-pulseaudio

sudo systemctl enable --now docker
sudo systemctl enable       greetd
