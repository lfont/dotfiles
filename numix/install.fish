if test (uname -s) = "Darwin"
then
  exit 0
end

. ./functions/install_repository.fish

mkdir -p ~/.themes

if test ! -d ~/.themes/Numix
  set install_dir (install_repository https://github.com/shimmerproject/Numix.git Numix)
  ln -sf $install_dir ~/.themes/
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

