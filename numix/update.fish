set dir $DOTFILES_INSTALL_DIR

for repo in $dir/Numix $dir/numix-icon-theme $dir/numix-icon-theme-circle
  cd $repo
  git pull
  cd -
end

