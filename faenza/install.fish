if test (uname -s) = "Darwin"
then
  exit 0
end

. ./functions/install_archive.fish

set -l NAME faenza-icon-theme
set -l VERSION 1.3

mkdir -p ~/.icons

if test ! -d ~/.icons/Faenza
  set install_dir (install_archive https://$NAME.googlecode.com/files/{$NAME}_{$VERSION}.zip $NAME)
  eval $install_dir/INSTALL
end

