function install_repository
  set -l url         $argv[1]
  set -l name        $argv[2]
  set -l install_dir $DOTFILES_INSTALL_DIR/$name

  mkdir $install_dir

  git clone $url $install_dir

  echo $install_dir
end

