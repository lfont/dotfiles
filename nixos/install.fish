if not uname -a | grep NixOS
  exit 0
end

sudo ln -sf (pwd)/nixos/hardware-configuration.nix /etc/nixos/
sudo ln -sf (pwd)/nixos/configuration.nix          /etc/nixos/

exit 0

