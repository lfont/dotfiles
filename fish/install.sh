# check for fish
if test ! $(which fish)
then
    echo " Installing fishell..."
    if [ "$(uname -s)" == "Darwin" ]
    then
        ../homebrew/install.sh
        brew install fish

        echo $(which fish) >> /etc/shells
    elif test ! $(which nixos-install)
    then
        #sudo apt-add-repository ppa:fish-shell/release-2
        sudo apt-get update
        sudo apt-get install fish
    fi
fi

# restore fish configuration
mkdir -p ~/.config/fish
ln -sf $(pwd)/fish/config.fish ~/.config/fish/

# set fish as the default shell
if [ $SHELL != $(which fish) ]
then
    chsh -s $(which fish)
fi

# install autojump
fish ./fish/install-autojump.fish

exit 0

