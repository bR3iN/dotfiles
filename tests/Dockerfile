FROM fedora:latest

RUN dnf makecache
RUN dnf install -y 'dnf-command(copr)'
RUN dnf copr enable -y agriffis/neovim-nightly
RUN dnf install -y fish make sudo neovim git python3-pip

COPY . /root/.dotfiles

WORKDIR /root/

ENTRYPOINT fish
