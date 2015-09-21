ln -sf $(pwd)/zsh/zshenv   ~/.zshenv
ln -sf $(pwd)/zsh/zprofile ~/.zprofile
ln -sf $(pwd)/zsh/zshrc    ~/.zshrc

if [ ! -d ~/.oh-my-zsh ]; then
    curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
fi

mkdir -p ~/.oh-my-zsh/custom/themes
ln -sf $(pwd)/zsh/fishy.zsh-theme ~/.oh-my-zsh/custom/themes/

mkdir -p ~/.oh-my-zsh/custom/plugins
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git \
          ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
