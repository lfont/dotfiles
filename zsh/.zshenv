# -*-sh-*-

## Uni* password manager
export PASSWORD_STORE_DIR=~/AppData/pass

# Check if $LANG is badly set as it causes issues
if [[ $LANG == "C" || $LANG == "" ]]; then
    >&2 echo "$fg[red]The \$LANG variable is not set. This can cause a lot of problems.$reset_color"
fi

## Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
    export EDITOR="vim"
elif [[ -n $VISUAL ]]; then
    export EDITOR=$VISUAL
else
    export EDITOR="emacsclient -t -a mg"
fi

## Compilation flags
# export ARCHFLAGS="-arch x86_64"

## SSH
# export SSH_KEY_PATH="~/.ssh/dsa_id"

## GnuPG
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

## OpenBSD's packages
if [[ $(uname) == OpenBSD ]]; then
   export PKG_PATH=http://ftp.fr.openbsd.org/pub/OpenBSD/snapshots/packages/`machine -a`/
fi

## Nix environment
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
   . ~/.nix-profile/etc/profile.d/nix.sh
fi

export NIX_PATH=${NIX_PATH:+$NIX_PATH:}unstable=/home/loic/.nix-defexpr/channels/unstable:frzpkgs=/home/loic/code/Fasterize/frzpkgs

## Docker
export DOCKER_HOST=tcp://127.0.0.1:2375
