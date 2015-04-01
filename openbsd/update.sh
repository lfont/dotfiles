ARCH=$(machine -a)

VERSION=57

KERNELS="bsd.rd
         bsd.mp"

SETS="xserv${VERSION}.tgz
      xfont${VERSION}.tgz
      xshare${VERSION}.tgz
      xbase${VERSION}.tgz
      comp${VERSION}.tgz
      man${VERSION}.tgz
      base${VERSION}.tgz"

CWD=$(pwd)
DEST=$DOTFILES_TMP_DIR/openbsd/$ARCH

rm -rf $DEST
mkdir $DEST
cd $DEST

if [[ $1 == "-k" ]]; then
    for kernel in $KERNELS; do
        curl -OL http://ftp.fr.openbsd.org/pub/OpenBSD/snapshots/$ARCH/$kernel
    done

    sudo rm /obsd; sudo cp /bsd /obsd && sudo cp bsd.mp /bsd
    sudo cp bsd.rd /
elif [[ $1 == "-s" ]]; then
    for set in $SETS; do
        curl -OL http://ftp.fr.openbsd.org/pub/OpenBSD/snapshots/$ARCH/$set
    done

    for set in $SETS; do
        sudo tar -C / -xzphf $set
    done
else
    cd /dev
    sudo ./MAKEDEV all
    cd -

    sudo installboot -v sd0
    sudo sysmerge
    sudo pkg_add -u
fi

cd $CWD
