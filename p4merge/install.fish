if test (which p4merge)
  exit 0
end

. ./functions/install_archive.fish
. ./functions/link_command.fish
. ./functions/link_application_icon.fish
. ./functions/link_desktop_file.fish

set NAME    p4merge
set VERSION r13.4
set ARCH    linux26x86_64
set PKG     p4v.tgz

set install_dir (install_archive http://www.perforce.com/downloads/perforce/$VERSION/bin.$ARCH/$PKG $NAME)

link_command $install_dir/bin/p4merge p4merge
link_application_icon $install_dir/lib/p4v/P4VResources/icons/P4-Merge_96x96-badge.png p4merge.png

ln -s (pwd)/p4merge/p4merge.desktop $install_dir/
link_desktop_file $install_dir/p4merge.desktop

