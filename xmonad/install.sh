# udev rules
sudo ln -sf $(pwd)/udev/51-android.rules /etc/udev/rules.d/

# X
ln -sf $(pwd)/x11/Xresources ~/.Xresources
ln -sf $(pwd)/x11/xinitrc    ~/.xinitrc
ln -sf $(pwd)/x11/xsession   ~/.xsession
ln -sf $(pwd)/x11/x11-autostart.sh ~/bin/x11-autostart

if [ ! -e /usr/share/xsessions/xsession.desktop ]; then
    sudo cp $(pwd)/x11/xsession.desktop /usr/share/xsessions/
fi

# xmonad
mkdir -p ~/.xmonad
ln -sf $(pwd)/xmonad/xmonad.hs ~/.xmonad/

# dmenu
mkdir -p ~/.config/dmenu
ln -sf $(pwd)/dmenu/dmenu-bind.sh ~/.config/dmenu/

# xmobar
ln -sf $(pwd)/xmobar/xmobarrc ~/.xmobarrc

# stalonetray
ln -sf $(pwd)/stalonetray/stalonetrayrc ~/.stalonetrayrc

# gtk configuration
ln -sf $(pwd)/xmonad/gtkrc-2.0 ~/.gtkrc-2.0

exit 0
