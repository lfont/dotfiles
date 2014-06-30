if test -d ~/.spf13-vim-3
    exit 0
end

ln -sf (pwd)/vim/gvimrc.local        ~/.gvimrc.local
ln -sf (pwd)/vim/vimrc.bundles.local ~/.vimrc.bundles.local
ln -sf (pwd)/vim/vimrc.local         ~/.vimrc.local

curl http://j.mp/spf13-vim3 -L -o - | sh

exit 0

