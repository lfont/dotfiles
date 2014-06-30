cd ~/.spf13-vim-3/
git pull
vim +BundleInstall! +BundleClean +q
cd -

cd ~/.vim/bundle/tern_for_vim/
npm update > $DOTFILES_TMP_DIR/tern_for_vim-install.log
cd -

cd ~/.vim/bundle/YouCompleteMe/
# FIXME: build.sh should be patched for NixOS
./install.sh > $DOTFILES_TMP_DIR/YouCompleteMe-install.log
cd -

