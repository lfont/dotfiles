#!/usr/bin/env bash

. ./script/env.sh
. ./script/functions.sh

mkdir -p ~/.themes

if [ ! -d ~/.themes/Numix ]; then
    install_dir=$(f_install_repository https://github.com/shimmerproject/Numix.git Numix)
    ln -sf "$install_dir" ~/.themes/
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
