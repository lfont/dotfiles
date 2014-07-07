if test (which firefox-nightly)
  exit 0
end

if test ! (which nix-env)
  . ./functions/install_archive.fish
  . ./functions/link_command.fish
  . ./functions/link_application_icon.fish
  . ./functions/link_desktop_file.fish

  set NAME    firefox-nightly
  set VERSION 33.0a1
  set ARCH    linux-x86_64
  set PKG     firefox-$VERSION.en-US.$ARCH.tar.bz2

  set install_dir (install_archive http://ftp.mozilla.org/pub/mozilla.org/firefox/nightly/latest-trunk/$PKG $NAME)

  link_command $install_dir/firefox firefox-nightly
  link_application_icon $install_dir/browser/icons/mozicon128.png firefox-nightly.png

  ln -s (pwd)/firefox/firefox-nightly.desktop $install_dir/
  link_desktop_file $install_dir/firefox-nightly.desktop
else
    nix-env -if (pwd)/firefox/firefox-bin-nightly.nix
end

