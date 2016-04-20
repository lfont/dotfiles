#!/usr/bin/env bash

. ./script/env.sh
. ./script/functions.sh

repo=$(f_install_repository https://github.com/joelthelion/autojump.git autojump)
cd $repo && ./install.py > $DOTFILES_TMP_DIR/autojump-install.log; cd -
