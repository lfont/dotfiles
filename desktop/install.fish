. ./functions/link_command.fish
. ./functions/install_repository.fish

set -l ROOT (pwd)/desktop

# X
ln -sf $ROOT/xsession ~/.xsession

# Mackbook pro keyboard backlight
link_command $ROOT/keyboard-backlight.fish keyboard-backlight

# Pulseaudio volume
link_command $ROOT/audio-volume.fish audio-volume

# openbox
mkdir -p ~/.config/openbox
ln -sf $ROOT/autostart ~/.config/openbox/
ln -sf $ROOT/menu.xml  ~/.config/openbox/
ln -sf $ROOT/rc.xml    ~/.config/openbox/

# compton
mkdir -p ~/.config/compton
ln -sf $ROOT/compton.conf ~/.config/compton/

# conky
ln -sf $ROOT/conkyrc ~/.conkyrc

# gmrun
ln -sf $ROOT/gmrunrc ~/.gmrunrc

# dmenu
mkdir -p ~/.config/dmenu
ln -sf $ROOT/dmenu-bind.sh ~/.config/dmenu/

# tint2
mkdir -p ~/.config/tint2
ln -sf $ROOT/tint2rc ~/.config/tint2/

# terminator
mkdir -p ~/.config/terminator
ln -sf $ROOT/config ~/.config/terminator/

# pytyle2
mkdir -p ~/.config/pytyle2
ln -sf $ROOT/config.ini ~/.config/pytyle2/
sudo apt install python-xpyb
hg clone https://code.google.com/p/pytyle/ $DOTFILES_INSTALL_DIR/pytyle2
cd $DOTFILES_INSTALL_DIR/pytyle2
sudo python2 setup.py install
cd -

exit 0

