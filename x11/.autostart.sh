#!/usr/bin/env bash

# background services
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
syncthing -no-browser -logflags=3 &
#btsync --config ~/.config/btsync/sync.conf
pcmanfm --daemon-mode &

if [ "$1" = "-e" ]; then
  emacs --daemon
fi

# systray
xfce4-power-manager &
nm-applet &
clipit &
blueman-applet &
pasystray &
