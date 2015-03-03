. ./functions/link_command.fish
. ./functions/install_repository.fish

# udev rules
sudo ln -sf (pwd)/udev/51-android.rules /etc/udev/rules.d/

# X
ln -sf (pwd)/x11/Xresources ~/.Xresources
ln -sf (pwd)/x11/xinitrc    ~/.xinitrc
ln -sf (pwd)/x11/xsession   ~/.xsession

if ! test -e /usr/share/xsessions/xsession.desktop
    sudo cp (pwd)/x11/xsession.desktop /usr/share/xsessions/
end

# xmonad
mkdir -p ~/.xmonad
ln -sf (pwd)/xmonad/xmonad.hs ~/.xmonad/

# dmenu
mkdir -p ~/.config/dmenu
ln -sf (pwd)/dmenu/dmenu-bind.sh ~/.config/dmenu/

# xmobar
ln -sf (pwd)/xmobar/xmobarrc ~/.xmobarrc

# stalonetray
ln -sf (pwd)/stalonetray/stalonetrayrc ~/.stalonetrayrc

exit 0
