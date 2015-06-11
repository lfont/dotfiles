# udev rules
sudo ln -sf $(pwd)/udev/51-android.rules /etc/udev/rules.d/

# X
ln -sf $(pwd)/x11/Xresources   ~/.Xresources
ln -sf $(pwd)/x11/xinitrc      ~/.xinitrc
ln -sf $(pwd)/x11/xsession     ~/.xsession
ln -sf $(pwd)/x11/xsidle.sh    ~/bin/
ln -sf $(pwd)/x11/gtkrc-2.0    ~/.gtkrc-2.0
mkdir -p ~/.config/gtk-3.0/
ln -sf $(pwd)/x11/settings.ini ~/.config/gtk-3.0/

if [ ! -e /usr/share/xsessions/xsession.desktop ]; then
    sudo cp $(pwd)/x11/xsession.desktop /usr/share/xsessions/
fi

# hw helpers
ln -sf $(pwd)/hw/audio-volume.sh ~/bin/
ln -sf $(pwd)/hw/dbus-power.sh   ~/bin/halt
ln -sf $(pwd)/hw/dbus-power.sh   ~/bin/reboot
ln -sf $(pwd)/hw/dbus-power.sh   ~/bin/zzz
ln -sf $(pwd)/hw/dbus-power.sh   ~/bin/ZZZ

# xmonad
mkdir -p ~/.xmonad
ln -sf $(pwd)/xmonad/xmonad.hs         ~/.xmonad/
ln -sf $(pwd)/xmonad/xmonad-start-once.sh ~/bin/

# dmenu
ln -sf $(pwd)/dmenu/dmenu-bind.sh ~/bin/

# xmobar
ln -sf $(pwd)/xmobar/xmobarrc ~/.xmobarrc

# conky
ln -sf $(pwd)/conky/conky-xmobar.sh ~/bin/

# stalonetray
ln -sf $(pwd)/stalonetray/stalonetrayrc ~/.stalonetrayrc

exit 0
