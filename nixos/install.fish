if not uname -a | grep NixOS
  exit 0
end

. ./functions/link_command.fish

sudo ln -sf (pwd)/nixos/hardware-configuration.nix /etc/nixos/
sudo ln -sf (pwd)/nixos/configuration.nix          /etc/nixos/

link_command (pwd)/nixos/keyboard-backlight.fish keyboard-backlight
link_command (pwd)/nixos/audio-volume.fish audio-volume

exit 0

