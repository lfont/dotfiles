fish ./vim/install-spf13.fish

# Tagbar (https://github.com/majutsushi/tagbar/wiki)
# <leader>tt, ctrl-], Ctrl-T
if test ! (which nix-env)
  sudo apt install exuberant-ctags
  sudo npm install -g git://github.com/ramitos/jsctags.git
else
  nix-env -i ctags
  # FIXME: How to install this npm package globally?
  cd ~bin
  npm install git://github.com/ramitos/jsctags.git
  ln -s ~/bin/node_modules/.bin/jsctags ~/bin/
  cd -
end

exit 0

