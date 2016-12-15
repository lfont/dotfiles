# Enhance path
PATH=$HOME/bin:$PATH

if [ "$EUID" -ne 0 ]; then
  PATH=$PATH:/sbin:/usr/sbin:/usr/local/sbin
fi

export PATH

# Nix environment
[ -s ~/.nix-profile/etc/profile.d/nix.sh ] \
  && source ~/.nix-profile/etc/profile.d/nix.sh

export NIX_PATH=${NIX_PATH:+$NIX_PATH:}unstable=${HOME}/.nix-defexpr/channels/unstable:frzpkgs=${HOME}/code/Fasterize/frzpkgs

# Guix environment
xset +fp $HOME/.guix-profile/share/fonts/truetype
export GUIX_PROFILE="$HOME/.guix-profile"
export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
[ -s "$HOME/.guix-profile/etc/profile" ] \
  && source "$HOME/.guix-profile/etc/profile"

# Common bash setup
[ -s ~/.bashrc ] && source ~/.bashrc

# Check if $LANG is badly set as it causes issues
if [[ $LANG == "C" || $LANG == "" ]]; then
  >&2 echo "${BRIGHT}${RED}The \$LANG variable is not set. This can cause a lot of problems.${RESET}"
fi

# Launch a gpg-agent instance
if [[ $(gpg-agent --version | head -n 1 | awk '{print $3}') < 2.1 ]]; then
  if ! pgrep gpg-agent > /dev/null; then
    eval $(gpg-agent --daemon --sh --write-env-file ~/.gpg-agent-info)
  fi
fi

# Launch a gnome-keyring instance
if command -v gnome-keyring-daemon > /dev/null; then
  if ! pgrep gnome-keyring-daemon > /dev/null; then
    eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
    export SSH_AUTH_SOCK
  fi
fi