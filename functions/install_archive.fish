function install_archive
  set -l url         $argv[1]
  set -l name        $argv[2]
  set -l tmp_dir     $DOTFILES_TMP_DIR/$name
  set -l install_dir $DOTFILES_INSTALL_DIR/$name

  mkdir $tmp_dir
  mkdir $install_dir

  rm -r $tmp_dir/*
  wget -P $tmp_dir $url

  tar -C $install_dir -xjvf $tmp_dir/*.tar.bz2 --strip-components 1 > $DOTFILES_TMP_DIR/{$name}-extract.log
  tar -C $install_dir -xzvf $tmp_dir/*.tar.gz  --strip-components 1 > $DOTFILES_TMP_DIR/{$name}-extract.log
  tar -C $install_dir -xzvf $tmp_dir/*.tgz     --strip-components 1 > $DOTFILES_TMP_DIR/{$name}-extract.log

  echo $install_dir
end

