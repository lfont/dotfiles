# -*-sh-*-
. ~/.xprofile

## Set favorite DE behavior
export DE="lxde"
export DESKTOP_SESSION="LXDE"
export XDG_MENU_PREFIX="lxde-"

## Default X apps
export TERMINAL="urxvt -e bash -c 'tmux attach -t term || tmux new -s term'"
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

function execwm
{
  local wm=$@
  if command -v systemctl >/dev/null; then
      exec "$@"
  else
    exec ck-launch-session "$@"
  fi
}

session=${1:-cwm}

case $session in
  cwm)
    tint2 &
    dex -ae cwm
    execwm cwm;;
  exwm)
    xhost +SI:localuser:$USER
    execwm emacs --eval "(with-eval-after-load \"init-exwm\" (exwm-enable))";;
  fluxbox)
    execwm startfluxbox;;
  i3)
    execwm i3;;
  # No known session, try to run it as command
  *) exec $1;;
esac
