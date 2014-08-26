function install_archive
  set -l url         $argv[1]
  set -l name        $argv[2]
  set -l tmp_dir     $DOTFILES_TMP_DIR/$name
  set -l install_dir $DOTFILES_INSTALL_DIR/$name

  mkdir $tmp_dir
  mkdir $install_dir

  rm -r $tmp_dir/*

  mkdir $tmp_dir/download
  wget -P $tmp_dir/download $url

  mkdir $tmp_dir/extract

  if bzip2 -t $tmp_dir/download/*
    tar -C $tmp_dir/extract -xjvf $tmp_dir/download/* > $DOTFILES_TMP_DIR/{$name}-extract.log
  else
    tar -C $tmp_dir/extract -xzvf $tmp_dir/download/* > $DOTFILES_TMP_DIR/{$name}-extract.log
  end

  if test (ls $tmp_dir/extract | wc -l) -eq 1
    mv $tmp_dir/extract/*/* $install_dir/
  else if test (ls -a $tmp_dir/extract | wc -l) -eq 3
    mv $tmp_dir/extract/.*/* $install_dir/
  else
    mv $tmp_dir/extract/* $install_dir/
  end

  echo $install_dir
end

