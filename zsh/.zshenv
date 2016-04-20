# -*-sh-*-

export DE="lxde"
export DESKTOP_SESSION="LXDE"
export XDG_MENU_PREFIX="lxde-"

export VISUAL="emacsclient -c -a mg"
export BROWSER="env GTK_THEME=Adwaita firefox"

export QT_SELECT=4

export PASSWORD_STORE_DIR=~/AppData/pass

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR="vim"
else
  export EDITOR=$VISUAL
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# gnupg
export GPG_TTY=$(tty)

if [ -f ~/.gpg-agent-info ]; then
   . ~/.gpg-agent-info
fi
#else
#    unset SSH_AGENT_PID
#    if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
#        export SSH_AUTH_SOCK="${HOME}/.gnupg/S.gpg-agent.ssh"
#    fi
#fi

# OpenBSD's packages
if [[ $(uname) == OpenBSD ]]; then
   export PKG_PATH=http://ftp.fr.openbsd.org/pub/OpenBSD/snapshots/packages/`machine -a`/
fi

# Nix environment
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
   . ~/.nix-profile/etc/profile.d/nix.sh
fi

# Docker
export DOCKER_HOST=tcp://127.0.0.1:2375
