. ./script/functions.sh

NAME=syncthing
VERSION=0.10.31
ARCH=linux-amd64

install_dir=$(f_install_archive https://github.com/syncthing/syncthing/releases/download/v$VERSION/$NAME-$ARCH-v$VERSION.tar.gz $NAME)
f_link_command $install_dir/$NAME $NAME
