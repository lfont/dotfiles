if [ $(uname -a | grep NixOS) ]; then
    exit 0
fi

sudo ln -sf $(pwd)/nixos/hardware-configuration.nix /etc/nixos/
sudo ln -sf $(pwd)/nixos/configuration.nix          /etc/nixos/

exit 0
