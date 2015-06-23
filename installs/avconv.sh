#!/bin/bash
# -*- coding: utf-8-unix -*-
set -eu

BASE="$(cd "$(dirname $0)"; pwd)"
. "${BASE}/commons.sh"

YASM_TAR_GZ='http://www.tortall.net/projects/yasm/releases/yasm-1.3.0.tar.gz'
AAC_GIT='git://git.code.sf.net/p/opencore-amr/fdk-aac'
X264_GIT='git://git.videolan.org/x264.git'
X265_HG='https://bitbucket.org/multicoreware/x265'
AV_GIT='git://git.libav.org/libav.git'

TMP_DIR="/tmp/kui-avconv"
JOBS=1

main() {
    if not has porg; then
        cd "$BASE"
        ./porg.sh
    else
        echo_green "Already install porg"
    fi

    if [[ -e '/proc/cpuinfo' ]]; then
        local cpus=$(cat /proc/cpuinfo | grep '^processor' | wc -l)
        JOBS=$(printf "%.0f" $(echo "$cpus * 1.5" | bc))
        echo_green 'JOBS='$JOBS
    fi

    if is_ubuntu; then
        run sudo apt-get install -y cmake cmake-curses-gui build-essential autoconf mercurial
    # elif is_mac_os_x; then
        # true
    else
        echo "Non supported platform" >&2
        exit 1
    fi

    install_yasm
    install_libx264
    # install_libx265
    install_libfdk_aac
    install_libav

    sudo ldconfig
}

print_head() {
    echo_green '############################################################'
    echo_green "### $@"
    echo_green '############################################################'
}


install_yasm() {
    if porg -a | grep -qF 'yasm-1.3'; then
        echo_green 'Skip: Already installed yasm'
        return
    fi

    print_head 'install yasm'

    local workspace="$TMP_DIR/yasm"
    local tar_gz="$(basename "$YASM_TAR_GZ")"

    mkdir -p "$workspace"
    cd "$workspace"
    run pwd
    if [[ ! -f "$tar_gz" ]]; then
        run wget "$YASM_TAR_GZ"
    fi
    run tar xzf "$tar_gz"
    cd "${tar_gz%.tar.gz}"
    run pwd

    run ./configure
    run make clean
    run make -j $JOBS
    run sudo porg -lD 'make install'

    return 0
}

install_libfdk_aac() {
    print_head 'install libfdk-aac'

    local workspace="$TMP_DIR/libfdk-aac"

    if [[ -e "$workspace/.git" ]]; then
        cd "$workspace"
        run pwd
        run git pull
    else
        run rm -fr "$workspace"
        run git clone --depth=1 --branch=master "$AAC_GIT" "$workspace"
        cd "$workspace"
        run pwd
    fi

    run autoreconf -fiv
    run ./configure \
        --enable-shared
    run make clean
    run make -j $JOBS
    run sudo porg -lp 'libfdk-aac-git' 'make install-strip'

    return 0
}

install_libx264() {
    print_head 'install libx264'

    local workspace="$TMP_DIR/libx264"

    if [[ -e "$workspace/.git" ]]; then
        cd "$workspace"
        run pwd
        run git pull
    else
        run rm -fr "$workspace"
        run git clone --depth=1 --branch=master "$X264_GIT" "$workspace"
        cd "$workspace"
        run pwd
    fi

    run ./configure \
        --enable-shared \
        --disable-ffms \
        --disable-gpac \
        --disable-lavf \
        --disable-swscale
    run make clean
    run make -j $JOBS
    run sudo porg -lp 'x264-git' 'make install'

    return 0
}

install_libx265() {
    print_head 'install libx265'

    if not is_ubuntu; then
        echo "Non supported platform" >&2
        exit 1
    fi

    local workspace="$TMP_DIR/x265"
    run rm -fr "$workspace"
    run hg clone "$X265_HG" "$workspace"
    cd "$workspace"
    run pwd

    # see https://bitbucket.org/multicoreware/x265/wiki/Home

    cd build/linux
    run pwd
    run ./make-Makefiles.bash
    run make clean
    run make -j $JOBS
    run sudo porg -lp 'x265-hg' 'make install'
}

install_libav() {
    print_head 'install libav'

    local workspace="$TMP_DIR/libav"

    if [[ -e "$workspace/.git" ]]; then
        cd "$workspace"
        run pwd
        run git pull
    else
        run rm -fr "$workspace"
        run git clone --depth=1 --branch=master "$AV_GIT" "$workspace"
        cd "$workspace"
        run pwd
    fi

    run ./configure \
        --enable-shared \
        --enable-nonfree \
        --enable-asm \
        --enable-libx264 \
        --enable-libfdk-aac \
        --enable-gpl
    run make clean
    run make -j $JOBS
    run sudo porg -lp 'libav-git' 'make install'

    return 0
}

main
