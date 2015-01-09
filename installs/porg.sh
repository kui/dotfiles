#!/bin/bash
# -*- coding: utf-8-unix -*-
set -eu

BASE="$(cd "$(dirname $0)"; pwd)"
. "${BASE}/commons.sh"

if is_ubuntu; then
    run sudo apt-get install build-essential gcc g++
elif is_mac_os_x; then
    true
else
    echo "Non supported platform" >&2
    exit 1
fi

TMP="/tmp/porg"
run rm -frv "$TMP"
run mkdir -p "$TMP"

cd "$TMP"
run pwd
run wget "http://downloads.sourceforge.net/project/porg/porg-0.7.tar.gz"
run tar zxf porg-0.7.tar.gz

cd porg-0.7
run pwd
run ./configure --disable-grop
run make
run sudo make install

echo "Success"
