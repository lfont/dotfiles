. ./script/functions.sh

curl https://raw.githubusercontent.com/creationix/nvm/v0.25.1/install.sh | bash
f_link_command $(pwd)/nvm/node-bin.sh node
f_link_command $(pwd)/nvm/node-bin.sh tern
