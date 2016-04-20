#!/usr/bin/env bash

. ./script/env.sh
. ./script/functions.sh

NAME=firefox-aurora
VERSION=40.0a2
ARCH=linux-x86_64
PKG=firefox-$VERSION.en-US.$ARCH.tar.bz2
URL=http://ftp.mozilla.org/pub/mozilla.org/firefox/nightly/latest-mozilla-aurora/$PKG

install_dir=$(f_install_archive $URL $NAME)

f_link_command $install_dir/firefox firefox-aurora
f_link_application_icon $install_dir/browser/icons/mozicon128.png $NAME.png
