if test -d $DOTFILES_INSTALL_DIR/autojump
  exit 0
end

. ./functions/install_repository.fish

set repo (install_repository 'https://github.com/joelthelion/autojump.git' autojump)
cd $repo
./install.py > $DOTFILES_TMP_DIR/autojump-install.log
cd -

exit 0

