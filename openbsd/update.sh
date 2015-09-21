ARCH=$(machine -a)

VERSION=58

URL=http://ftp.fr.openbsd.org/pub/OpenBSD/snapshots/$ARCH

KERNELS="bsd.rd
         bsd.mp"

SETS="xserv${VERSION}.tgz
      xfont${VERSION}.tgz
      xshare${VERSION}.tgz
      xbase${VERSION}.tgz
      comp${VERSION}.tgz
      man${VERSION}.tgz
      game${VERSION}.tgz
      base${VERSION}.tgz"

CWD=$(pwd)
DEST=$DOTFILES_TMP_DIR/openbsd/$ARCH

rm -rf $DEST
mkdir -p $DEST
cd $DEST

function check_signature() {
    if [ ! -e SHA256.sig ]; then
        echo "-> downloading signature"
        curl -OL $URL/SHA256.sig
    fi

    echo "-> verifying file: $1"
    signify -C -p /etc/signify/openbsd-$VERSION-base.pub -x SHA256.sig $1
}

if [[ $1 == "-k" ]]; then
    for kernel in $KERNELS; do
        echo "-> downloading: $kernel"
        curl -OL $URL/$kernel
        check_signature $kernel || exit
    done

    echo "-> copying: kernel"
    sudo rm /obsd; sudo cp /bsd /obsd && sudo cp bsd.mp /bsd

    echo "-> copying: ramdisk"
    sudo cp bsd.rd /
elif [[ $1 == "-s" ]]; then
    for set in $SETS; do
        echo "-> downloading: $set"
        curl -OL $URL/$set
        check_signature $set || exit
    done

    for set in $SETS; do
        echo "-> extracting: $set"
        sudo tar -C / -xzphf $set
    done
else
    cd /dev
    echo "-> making devices nodes"
    sudo ./MAKEDEV all
    cd -

    echo "-> installing bootloader"
    sudo installboot -v sd0

    echo "-> merging system files"
    sudo sysmerge

    echo "-> updating packages"
    sudo pkg_add -Uu

    echo "-> merging packages files"
    sudo sysmerge -p
fi

cd $CWD
