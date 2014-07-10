. ./functions/link_command.fish

if test -d ~/.nix-profile/bin
    for n in (ls ~/.nix-profile/bin/nix-*)
        link_command (pwd)/nix/nix-bash-context.sh (echo (basename $n))
    end
end

if test (which nix-env)
  exit 0
end

bash -c (curl https://nixos.org/nix/install)

exit 0

