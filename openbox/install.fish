. ./functions/link_command.fish
. ./functions/install_repository.fish

# udev rules
sudo ln -sf (pwd)/udev/51-android.rules  /etc/udev/rules.d/
sudo ln -sf (pwd)/udev/100-android.rules /etc/udev/rules.d/

# X
rm ~/.xprofile
ln -sf (pwd)/x11/xinitrc          ~/.xinitrc
ln -sf (pwd)/x11/Xresources       ~/.Xresources
ln -sf (pwd)/x11/openbox-xsession ~/.xsession

# Mackbook pro keyboard backlight
link_command (pwd)/hw/keyboard-backlight.fish keyboard-backlight

# Pulseaudio volume
link_command (pwd)/hw/audio-volume.fish audio-volume

# openbox
mkdir -p ~/.config/openbox
ln -sf (pwd)/openbox/autostart ~/.config/openbox/
ln -sf (pwd)/openbox/menu.xml  ~/.config/openbox/
ln -sf (pwd)/openbox/rc.xml    ~/.config/openbox/

# compton
mkdir -p ~/.config/compton
ln -sf (pwd)/compton/compton.conf ~/.config/compton/

# conky
ln -sf (pwd)/conky/conkyrc ~/.conkyrc

# gmrun
ln -sf (pwd)/gmrun/gmrunrc ~/.gmrunrc

# dmenu
mkdir -p ~/.config/dmenu
ln -sf (pwd)/dmenu/dmenu-bind.sh ~/.config/dmenu/

# tint2
mkdir -p ~/.config/tint2
ln -sf (pwd)/tint2/tint2rc ~/.config/tint2/

# terminator
mkdir -p ~/.config/terminator
ln -sf (pwd)/terminator/config ~/.config/terminator/

# pytyle2
mkdir -p ~/.config/pytyle2
ln -sf (pwd)/pytyle2/config.ini ~/.config/pytyle2/
sudo apt install python-xpyb
hg clone https://code.google.com/p/pytyle/ $DOTFILES_INSTALL_DIR/pytyle2
cd $DOTFILES_INSTALL_DIR/pytyle2
sudo python2 setup.py install
cd -

exit 0

