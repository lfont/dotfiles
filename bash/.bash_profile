# Nix environment
[ -s ~/.nix-profile/etc/profile.d/nix.sh ] \
  && source ~/.nix-profile/etc/profile.d/nix.sh

export NIX_PATH=${NIX_PATH:+$NIX_PATH:}local=${HOME}/code/nixpkgs
export LOCALE_ARCHIVE=$HOME/.nix-profile/lib/locale/locale-archive
export LADSPA_PATH=$HOME/.nix-profile/lib/ladspa
export LV2_PATH=$HOME/.nix-profile/lib/lv2
export LXVST_PATH=$HOME/.nix-profile/lib/vst

# User bin are always the favorites
export PATH=$HOME/bin:$HOME/.local/bin:$PATH

# Non root user can see sbin
if [ "$EUID" -ne 0 ]; then
  export PATH=$PATH:/sbin:/usr/sbin:/usr/local/sbin
fi

# Common bash setup
[ -s ~/.bashrc ] \
  && source ~/.bashrc

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
    gnome-keyring-daemon --start --daemonize --components=pkcs11,secrets
  fi
fi
