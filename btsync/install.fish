if test (which btsync)
  exit 0
end

if test ! (which nix-env)
    . ./functions/install_archive.fish
    . ./functions/link_command.fish

    set NAME    btsync
    set VERSION stable
    set ARCH    linux-x64

    set install_dir (install_archive http://download-lb.utorrent.com/endpoint/$NAME/os/$ARCH/track/$VERSION $NAME)

    link_command $install_dir/$NAME $NAME
else
    nix-env -i btsync
end

mkdir -p ~/.sync
mkdir -p ~/.config/btsync
ln -s (pwd)/btsync/sync.conf ~/.config/btsync/
