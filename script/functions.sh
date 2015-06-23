function f_link_command () {
    cmd=$1
    link=$2

    ln -sf $cmd ~/bin/$link
}

function f_link_desktop_file () {
    desktop=$1

    ln -sf $desktop ~/.local/share/applications/
}

function f_link_application_icon () {
    icon=$1
    link=$2

    mkdir -p ~/.local/share/icons/hicolor/128x128/apps/

    ln -sf $icon ~/.local/share/icons/hicolor/128x128/apps/$link
    gtk-update-icon-cache ~/.local/share/icons/hicolor
}

function f_install_repository () {
    url=$1
    name=$2
    install_dir="$DOTFILES_INSTALL_DIR/$name"

    mkdir $install_dir
    git clone $url $install_dir

    echo $install_dir
}

function f_install_archive () {
    url=$1
    name=$2
    tmp_dir=$DOTFILES_TMP_DIR/$name
    install_dir=$DOTFILES_INSTALL_DIR/$name

    mkdir $tmp_dir
    mkdir $install_dir

    rm -r $tmp_dir/*

    mkdir $tmp_dir/download
    wget -P $tmp_dir/download $url

    mkdir $tmp_dir/extract

    if [ $(bzip2 -t $tmp_dir/download/* &>/dev/null; echo $?) -eq 0 ]; then
        tar -C $tmp_dir/extract -xjvf $tmp_dir/download/* > $DOTFILES_TMP_DIR/$name-extract.log
    elif [ $(gzip -t $tmp_dir/download/* &>/dev/null; echo $?) -eq 0 ]; then
        tar -C $tmp_dir/extract -xzvf $tmp_dir/download/* > $DOTFILES_TMP_DIR/$name-extract.log
    else
        unzip $tmp_dir/download/* -d $tmp_dir/extract > $DOTFILES_TMP_DIR/$name-extract.log
    fi

    if [ $(ls $tmp_dir/extract | wc -l) -eq 1 ]; then
        mv $tmp_dir/extract/*/* $install_dir/
    elif [ $(ls -a $tmp_dir/extract | wc -l) -eq 3 ]; then
        mv $tmp_dir/extract/.*/* $install_dir/
    else
        mv $tmp_dir/extract/* $install_dir/
    fi

    echo $install_dir
}

function f_install_file () {
    url=$1
    sha256=$2
    file_path=$DOTFILES_INSTALL_DIR/$(basename $url)

    wget -P $DOTFILES_INSTALL_DIR $url

    if [ $(sha256sum $file_path | cut -d ' ' -f 1) == $sha256 ]; then
        echo $file_path
    else
        rm $file_path
        echo ''
    fi
}
