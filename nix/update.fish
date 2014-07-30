nix-channel --update
nix-env -uA nixpkgs.chromiumDev
nix-env -uA nixpkgs.kde4.kdiff3
nix-env -uA nixpkgs.emacs

exit 0

