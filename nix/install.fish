. ./functions/link_command.fish

if test ! -d ~/.nix-profile/bin
    curl https://nixos.org/nix/install | bash

    for n in (ls ~/.nix-profile/bin/nix-*)
        link_command (pwd)/nix/nix-bash-context.sh (echo (basename $n))
    end

    mkdir -p ~/.nixpkgs
    ln -sf (pwd)/nix/config.nix ~/.nixpkgs/
end

nix-env -i chromium-dev
nix-env -i kdiff3
nix-env -i emacs
nix-env -i viewnior
fish ./firefox/install.fish
fish ./btsync/install.fish

exit 0

