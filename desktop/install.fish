. ./functions/link_command.fish

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

exit 0

