# -*-sh-*-
export PATH="$HOME/bin:$PATH"

## Launches a gnome-keyring instance
if command -v gnome-keyring-daemon >/dev/null; then
    eval $(killall gnome-keyring-daemon)
    eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
    export SSH_AUTH_SOCK
fi

## Start an Emacs server (this works with emacs-lucid)
#if command -v emacs >/dev/null; then
#    if ! tmux has -t emacs; then
#        tmux new -s emacs -d "emacs -nw"
#    fi
#fi
