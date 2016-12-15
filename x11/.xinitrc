# -*-sh-*-
. ~/.xprofile

## Set favorite DE behavior
export DE="lxde"
export DESKTOP_SESSION="LXDE"
export XDG_MENU_PREFIX="lxde-"

## Default X apps
export BROWSER="firefox"
export VISUAL="emacsclient -c -a mg"
export QT_SELECT=4

## Source system init scripts
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

## Launches a session dbus instance
if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
    eval $(dbus-launch --sh-syntax --exit-with-session)
fi

xhost +SI:localuser:$USER

if command -v systemctl >/dev/null; then
  exec emacs --eval "(with-eval-after-load \"init-exwm\" (exwm-enable))"
else
  exec ck-launch-session emacs --eval "(with-eval-after-load \"init-exwm\" (exwm-enable))"
fi
