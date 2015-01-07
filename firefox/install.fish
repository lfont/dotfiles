. ./functions/link_desktop_file.fish
. ./firefox/env.fish

if test (which $NAME)
    exit 0
end

if test ! (which nix-env)
    . ./functions/install_archive.fish
    . ./functions/link_command.fish
    . ./functions/link_application_icon.fish

    set install_dir (install_archive $URL $NAME)

    link_command $install_dir/firefox $NAME
    link_application_icon $install_dir/browser/icons/mozicon128.png $NAME.png

    ln -s (pwd)/firefox/$NAME.desktop $install_dir/
    link_desktop_file $install_dir/$NAME.desktop
else
    fish ./firefox/update.fish

    if test ! (which nixos-install)
        link_desktop_file ~/.nix-profile/share/applications/$NAME.desktop
    end
end

exit 0

