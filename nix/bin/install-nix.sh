#!/usr/bin/env bash

if [ ! -d /nix ]; then
    curl https://nixos.org/nix/install | sh
fi
