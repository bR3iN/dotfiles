function update
    if [ -x /usr/bin/apt ]
        sudo apt update
        sudo apt upgrade -y
        sudo apt autoremove
    end

    if [ -x /usr/bin/dnf ]
        sudo dnf update -y
        sudo dnf autoremove
    end

    if [ -x /usr/bin/flatpak ]
        flatpak update
    end

    if [ -x /usr/bin/snap ]
        snap refresh
    end
end
