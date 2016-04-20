#!/usr/bin/env bash

. ./script/env.sh

repos="$DOTFILES_INSTALL_DIR/Numix
       $DOTFILES_INSTALL_DIR/numix-icon-theme
       $DOTFILES_INSTALL_DIR/numix-icon-theme-circle"

for repo in $repos; do
    cd $repo && { git pull; cd -; }
done
