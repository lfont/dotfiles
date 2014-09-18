if test (uname -s) = "Darwin"
then
  exit 0
end

. ./functions/install_repository.fish
. ./functions/install_archive.fish


mkdir -p ~/.themes

if test ! -d ~/.themes/Numix
  set install_dir (install_repository https://github.com/shimmerproject/Numix.git Numix)
  ln -sf $install_dir ~/.themes/
end

if test ! -d ~/.themes/waldorf
  set install_dir (install_repository https://github.com/xoraxiom/openbox-gtk-themes.git openbox-gtk-themes)
  for t in (ls $install_dir)
      if test -d $install_dir/$t
        ln -sf $install_dir/$t ~/.themes/
      end
  end
end


mkdir -p ~/.icons

if test ! -d ~/.icons/Numix
  set install_dir (install_repository https://github.com/numixproject/numix-icon-theme.git numix-icon-theme)
  ln -sf $install_dir/Numix ~/.icons/
  gtk-update-icon-cache -f -t ~/.icons/Numix
end

if test ! -d ~/.icons/Numix-Circle
  set install_dir (install_repository https://github.com/numixproject/numix-icon-theme-circle.git numix-icon-theme-circle)
  ln -sf $install_dir/Numix-Circle ~/.icons/
  gtk-update-icon-cache -f -t ~/.icons/Numix-Circle
end

# TODO: install from http://faenza-icon-theme.googlecode.com/svn/trunk/
if test ! -d ~/.icons/Faenza
  set install_dir (install_archive https://faenza-icon-theme.googlecode.com/files/faenza-icon-theme_1.3.zip faenza-icon-theme)
  eval $install_dir/INSTALL
end

