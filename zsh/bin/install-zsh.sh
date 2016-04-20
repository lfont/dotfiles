#!/usr/bin/env bash

if [ ! -d ~/.oh-my-zsh ]; then
    curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh
fi

mkdir -p ~/.oh-my-zsh/custom/plugins
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git \
          ~/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
