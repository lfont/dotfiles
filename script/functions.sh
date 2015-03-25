function f_install_repository() {
    url=$1
    name=$2
    install_dir="$DOTFILES_INSTALL_DIR/$name"

    mkdir $install_dir
    git clone $url $install_dir

    echo $install_dir
}
