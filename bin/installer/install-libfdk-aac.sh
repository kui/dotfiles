#!/bin/bash

set -eu

# debug
set -x

BASE_DIR="/tmp"

if [[ $EUID -ne 0 ]]
then
    echo "this script must be run as root" 1>&1
    exit 1
fi

main() {
    local workspace=$BASE_DIR/libfdk-aac

    if [[ -e "$workspace/.git" ]]
    then
        cd "$workspace"
        git pull
    else
        rm -fr "$workspace"
        git clone --depth=1 --branch=master \
            "git://git.code.sf.net/p/opencore-amr/fdk-aac" "$workspace"
        cd "$workspace"
    fi

    autoreconf -fiv
    ./configure --enable-shared
    make
    make install-strip

    return 0
}

main
