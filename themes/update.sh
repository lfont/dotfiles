repos="$DOTFILES_INSTALL_DIR/Numix
       $DOTFILES_INSTALL_DIR/Color-UI
       $DOTFILES_INSTALL_DIR/numix-icon-theme
       $DOTFILES_INSTALL_DIR/numix-icon-theme-circle
       $DOTFILES_INSTALL_DIR/slim-minimal"

for repo in $repos; do
    cd $repo && { git pull; cd -; }
done

sudo cp -r $DOTFILES_INSTALL_DIR/slim-minimal /usr/share/slim/themes/
