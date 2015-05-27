. ./script/functions.sh

mkdir -p ~/.themes

if [ ! -d ~/.themes/Numix ]; then
    install_dir=$(f_install_repository https://github.com/shimmerproject/Numix.git Numix)
    ln -sf "$install_dir" ~/.themes/
fi

if [ ! -d ~/.themes/Color-UI ]; then
    install_dir=$(f_install_repository https://github.com/UserContributer/Color-UI-theme-project.git Color-UI)
    ln -sf "$install_dir/Color-UI" ~/.themes/
fi


mkdir -p ~/.icons

if [ ! -d ~/.icons/Numix ]; then
    install_dir=$(f_install_repository https://github.com/numixproject/numix-icon-theme.git numix-icon-theme)
    ln -sf "$install_dir"/Numix ~/.icons/
    gtk-update-icon-cache -f -t ~/.icons/Numix
fi

if [ ! -d ~/.icons/Numix-Circle ]; then
    install_dir=$(f_install_repository https://github.com/numixproject/numix-icon-theme-circle.git numix-icon-theme-circle)
    ln -sf "$install_dir"/Numix-Circle ~/.icons/
    gtk-update-icon-cache -f -t ~/.icons/Numix-Circle
fi


if [ ! -d $DOTFILES_INSTALL_DIR/slim-minimal ]; then
    install_dir=$(f_install_repository https://github.com/naglis/slim-minimal.git slim-minimal)
    sudo cp -r $install_dir /usr/share/slim/themes/
fi
