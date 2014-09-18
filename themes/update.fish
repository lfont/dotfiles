set dir $DOTFILES_INSTALL_DIR

for repo in $dir/Numix $dir/numix-icon-theme $dir/numix-icon-theme-circle $dir/openbox-gtk-themes
  cd $repo
  git pull
  cd -
end

