# udev rules
sudo ln -sf $(pwd)/udev/51-android.rules /etc/udev/rules.d/

# X
ln -sf $(pwd)/x11/Xresources       ~/.Xresources
ln -sf $(pwd)/x11/xinitrc          ~/.xinitrc
ln -sf $(pwd)/x11/xsession         ~/.xsession
ln -sf $(pwd)/x11/x11-autostart.sh ~/bin/x11-autostart
ln -sf $(pwd)/x11/xsidle.sh        ~/bin/xsidle

if [ ! -e /usr/share/xsessions/xsession.desktop ]; then
    sudo cp $(pwd)/x11/xsession.desktop /usr/share/xsessions/
fi

# hw helpers
ln -sf $(pwd)/hw/audio-volume.sh ~/bin/audio-volume
ln -sf $(pwd)/hw/dbus-power.sh   ~/bin/halt
ln -sf $(pwd)/hw/dbus-power.sh   ~/bin/reboot
ln -sf $(pwd)/hw/dbus-power.sh   ~/bin/zzz
ln -sf $(pwd)/hw/dbus-power.sh   ~/bin/ZZZ

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
