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
    run sudo apt-get install automake autoconf libtinfo-dev libjpeg-dev \
        libpng-dev libgif-dev libtiff-dev libxaw7-dev libx11-dev
else
    echo "Not supported platform" >&2
    exit 1
fi

TMP="/tmp/emacs"
run rm -frv "$TMP"
run mkdir -pv "$TMP"

cd "$TMP"
run pwd
run wget http://ftpmirror.gnu.org/emacs/emacs-24.5.tar.xz
run tar xfJ emacs-24.5.tar.xz

cd "emacs-24.5"
run pwd
run ./autogen.sh
run ./configure
run make
run sudo porg -lp "emacs" "make install"
