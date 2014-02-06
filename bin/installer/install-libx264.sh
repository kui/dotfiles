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
    local workspace="$BASE_DIR/libx264"

    if [[ -e "$workspace/.git" ]]
    then
        cd "$workspace"
        git pull
    else
        rm -fr "$workspace"
        git clone --depth=1 --branch=master \
            "git://git.videolan.org/x264.git" "$workspace"
        cd "$workspace"
    fi

    ./configure \
        --enable-shared \
        --disable-asm \
        --disable-cli \
        --disable-ffms \
        --disable-gpac \
        --disable-lavf \
        --disable-swscale
    make
    make install

    return 0
}

main
