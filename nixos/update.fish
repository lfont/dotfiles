if not uname -a | grep NixOS
  exit 0
end

sudo nixos-rebuild switch --upgrade > $DOTFILES_TMP_DIR/nixos-update.log

exit 0

