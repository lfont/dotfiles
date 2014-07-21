. ./functions/link_command.fish

if test ! (which nix-env)
    bash -c (curl https://nixos.org/nix/install)
end

if test -d ~/.nix-profile/bin
    for n in (ls ~/.nix-profile/bin/nix-*)
        link_command (pwd)/nix/nix-bash-context.sh (echo (basename $n))
    end
end

mkdir -p ~/.nixpkgs
ln -sf (pwd)/nix/config.nix ~/.nixpkgs/

if test (which nixos-rebuild)
    exit 0
end

nix-env -iA nixpkgs.chromiumDev
nix-env -iA nixpkgs.kde4.kdiff3

exit 0

