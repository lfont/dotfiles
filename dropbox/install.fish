. ./functions/install_archive.fish
. ./functions/link_command.fish

set install_dir (install_archive "https://www.dropbox.com/download?plat=lnx.x86_64" dropbox)
ln -sf $install_dir ~/.dropbox-dist

wget -O $DOTFILES_INSTALL_DIR/dropbox.py "https://www.dropbox.com/download?dl=packages/dropbox.py"
link_command $DOTFILES_INSTALL_DIR/dropbox.py dropbox
chmod +x $DOTFILES_INSTALL_DIR/dropbox.py
