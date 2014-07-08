ln -sf (pwd)/vim/gvimrc.local        ~/.gvimrc.local
ln -sf (pwd)/vim/vimrc.before.local  ~/.vimrc.before.local
ln -sf (pwd)/vim/vimrc.bundles.local ~/.vimrc.bundles.local
ln -sf (pwd)/vim/vimrc.local         ~/.vimrc.local

if test -d ~/.spf13-vim-3
  exit 0
end

curl http://j.mp/spf13-vim3 -L -o - | sh

exit 0

