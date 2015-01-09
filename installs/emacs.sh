#!/bin/bash
# -*- coding:utf-8 -*-
set -eu

BASE="$(cd "$(dirname $0)"; pwd)"
. "${BASE}/commons.sh"

if not has porg; then
    cd "$BASE"
    ./porg.sh
else
    echo_green "Already install porg"
fi

if is_ubuntu; then
    run sudo apt-get build-dep emacs24 -y
    run sudo apt-get install automake autoconf
elif is_mac_os_x; then
    run sudo port install libxml2 ncurses pkgconfig texinfo automake autoconf
else
    echo "Non supported platform" >&2
    exit 1
fi

TMP="/tmp/emacs"
run rm -frv "$TMP"
run mkdir -pv "$TMP"

cd "$TMP"
run pwd
run wget http://ftpmirror.gnu.org/emacs/emacs-24.4.tar.xz
run tar xfJ emacs-24.4.tar.xz

cd "emacs-24.4"
run pwd
run ./autogen.sh
run ./configure
run make
run sudo porg -lD "make install"
