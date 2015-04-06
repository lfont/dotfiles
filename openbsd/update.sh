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
mkdir -p $DEST
cd $DEST

if [[ $1 == "-k" ]]; then
    for kernel in $KERNELS; do
        echo "downloading: $kernel"
        curl -OL http://ftp.fr.openbsd.org/pub/OpenBSD/snapshots/$ARCH/$kernel
    done

    echo "copying kernel"
    sudo rm /obsd; sudo cp /bsd /obsd && sudo cp bsd.mp /bsd

    echo "copying ramdisk"
    sudo cp bsd.rd /
elif [[ $1 == "-s" ]]; then
    for set in $SETS; do
        echo "downloading: $set"
        curl -OL http://ftp.fr.openbsd.org/pub/OpenBSD/snapshots/$ARCH/$set
    done

    for set in $SETS; do
        echo "extracting: $set"
        sudo tar -C / -xzphf $set
    done
else
    cd /dev
    echo "making devices nodes"
    sudo ./MAKEDEV all
    cd -

    echo "installing bootloader"
    sudo installboot -v sd0

    echo "merging system files"
    sudo sysmerge

    echo "updating packages"
    sudo pkg_add -Uu

    echo "merging packages files"
    sudo sysmerge -p
fi

cd $CWD
