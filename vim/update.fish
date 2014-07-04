cd ~/.spf13-vim-3/
git pull
vim +PluginInstall! +PluginClean +qall
cd -

cd ~/.vim/bundle/tern_for_vim/
npm update > $DOTFILES_TMP_DIR/tern_for_vim-install.log
cd -

if test (which nix-shell)
  nix-shell (pwd)/vim/youcompletemeenv.nix --command "cd ~/.vim/bundle/YouCompleteMe && ./install.sh"
else
  cd ~/.vim/bundle/YouCompleteMe/
  ./install.sh > $DOTFILES_TMP_DIR/YouCompleteMe-install.log
  cd -
end

