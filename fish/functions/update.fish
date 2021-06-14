function update
    sudo apt update
    sudo apt upgrade -y
    sudo apt autoremove
    flatpak update
end
