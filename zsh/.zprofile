# -*-sh-*-
export PATH="$HOME/bin:$PATH"

## Launch a gpg-agent instance
if [[ $(gpg-agent --version | head -n 1 | awk '{print $3}') < 2.1 ]]; then
    if ! pgrep gpg-agent >/dev/null; then
        eval $(gpg-agent --daemon --sh --write-env-file ~/.gpg-agent-info)
    fi
fi

## Launch a gnome-keyring instance
if command -v gnome-keyring-daemon >/dev/null; then
    if ! pgrep gnome-keyring-daemon >/dev/null; then
        eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
        export SSH_AUTH_SOCK
    fi
fi

## Start an Emacs server (this works with emacs-lucid)
#if command -v emacs >/dev/null; then
#    if ! tmux has -t emacs >/dev/null; then
#        tmux new -s emacs -d "emacs -nw"
#    fi
#fi
