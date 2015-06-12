PATH=$HOME/bin:$PATH

if [[ $EUID -ne 0 ]]; then
    PATH=$PATH:/sbin:/usr/sbin:/usr/local/sbin
fi

export PATH

export EDITOR=e
export VISUAL=ew

# Nix environment
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
   . ~/.nix-profile/etc/profile.d/nix.sh
fi

. ~/.bashrc
