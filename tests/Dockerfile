FROM fedora:latest

RUN dnf makecache
RUN dnf install -y fish python3 python3-pyyaml make sudo

COPY . /root/.dotfiles

WORKDIR /root/

ENTRYPOINT fish
