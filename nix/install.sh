if [ ! -d /nix ]; then
    curl https://nixos.org/nix/install | sh
fi

if [ ! -d ~/.nixpkgs ]; then
    mkdir ~/.nixpkgs
    ln -sf $(pwd)/nix/config.nix ~/.nixpkgs/
fi

nix-env -i chromium-dev

exit 0
