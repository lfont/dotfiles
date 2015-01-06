. ./functions/link_command.fish

if test ! -d ~/.nix-profile/bin
    curl https://nixos.org/nix/install | bash

    for n in (ls ~/.nix-profile/bin/nix-*)
        link_command (pwd)/nix/nix-bash-context.sh (echo (basename $n))
    end
end

mkdir -p ~/.nixpkgs
ln -sf (pwd)/nix/config.nix ~/.nixpkgs/

if test (which nixos-rebuild)
    exit 0
end

nix-env -i chromium-dev
nix-env -i kde4.kdiff3
nix-env -i emacs

exit 0

