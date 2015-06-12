if [ $(uname -a | grep NixOS) ]; then
    exit 0
fi

sudo nixos-rebuild switch --upgrade > $DOTFILES_TMP_DIR/nixos-update.log

exit 0
