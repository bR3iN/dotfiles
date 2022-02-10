#!/usr/bin/bash

podman build -t dotfiles-test -f tests/Dockerfile .
podman run -it --rm dotfiles-test
