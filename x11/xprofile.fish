## Desktop vars
set -gx DE "lxde"
set -gx DESKTOP_SESSION "LXDE"
set -gx XDG_MENU_PREFIX "lxde-"
set -gx BROWSER "firefox"

function export-sh-vars
    for i in $argv
        set -gx  (echo $i | sed 's/=.*//') (echo $i | sed 's/\w*=//')
    end
end

## Launches a session dbus instance
if not set -q DBUS_SESSION_BUS_ADDRESS
    export-sh-vars (dbus-launch --exit-with-session)
end

## GNUPG agent
eval (gpg-agent --sh --daemon --enable-ssh-support)
export-sh-vars (cat ~/.gpg-agent-info)
set -gx GPG_TTY (tty)

## Gnome polkit
/usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 &

## Remaps CapsLock
setxkbmap -layout us -variant intl -option ctrl:nocaps

## Set keyboard settings - 250 ms delay and 25 cps (characters per second) repeat rate.
## Adjust the values according to your preferances.
xset r rate 250 25 &

## Turn on/off system beep
xset b off &

## Synaptics touchpad
synclient ClickFinger2=3
synclient ClickFinger3=2
synclient RightButtonAreaLeft=0
synclient RightButtonAreaTop=0
synclient VertScrollDelta=-111
synclient HorizScrollDelta=-111
synclient VertHysteresis=40
synclient HorizHysteresis=40
synclient TapButton1=0
synclient TapButton2=0
synclient TapButton3=0
synclient PalmDetect=1

## Restore screen layout
if test -e ~/.screenlayout/default.sh
    ~/.screenlayout/default.sh &
end

## Set root window colour
hsetroot -solid "#2E3436" &

## Restore wallpaper
nitrogen --restore &

## Set urxvt settings
xrdb -merge ~/.Xresources &

## Start PCManFM Daemon
pcmanfm --daemon-mode &

## Start xscreensaver
xscreensaver -no-splash &

## Pulse Audio volume control
pasystray &

## Start Clipboard manager
clipit &

## Run blueman applet
blueman-applet &

## Run bittorrent sync
btsync --config .config/btsync/sync.conf &

## Run an emacs instance
emacs --daemon
