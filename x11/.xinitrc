# -*-sh-*-
. ~/.xprofile

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

function execwm
{
  local wm=$@
  if command -v systemctl >/dev/null; then
      exec "$@"
  else
    exec ck-launch-session "$@"
  fi
}

session=${1:-xmonad}

case $session in
  cwm)
    tint2 &
    dex -ae CWM
    execwm cwm;;
  exwm)
    xhost +SI:localuser:$USER
    dex -ae EXWM
    execwm emacs --eval "(with-eval-after-load \"init-exwm\" (exwm-enable))";;
  fluxbox)
    dex -ae FLUXBOX
    execwm startfluxbox;;
  i3)
    dex -ae I3
    execwm i3;;
  xmonad)
    dex -ae XMONAD
    execwm xmonad;;
  # No known session, try to run it as command
  *) exec $1;;
esac
