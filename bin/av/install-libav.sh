#!/bin/bash

set -eu

# debug
set -x

# ソースファイルの参照先
YASM_URL="http://www.tortall.net/projects/yasm/releases/yasm-1.2.0.tar.gz"
AAC_GIT="git://git.code.sf.net/p/opencore-amr/fdk-aac"
X264_GIT="git://git.videolan.org/x264.git"
AV_GIT="git://git.libav.org/libav.git"

# 利用するディレクトリ
TMP_DIR="/tmp/kui-avconv"
PREFIX="/usr/local/kui-avconv"

export CFLAGS="-I${PREFIX}/include"
export LDFLAGS="-L${PREFIX}/lib"
export PATH="${PREFIX}/bin:$PATH"

if [[ $EUID -ne 0 ]]; then
    echo "this script must be run as root" 1>&1
    exit 1
fi

main() {
    mkdir -p "$TMP_DIR" "$PREFIX"

    install_yasm
    install_libx264
    install_libfdk-aac
    install_libav
}

print_head() {
    local a
    a='############################################################'
    a="### $1"
    a='############################################################'
}

install_yasm() {
    print_head 'install yasm'

    local workspace="$TMP_DIR/yasm"
    local tar_gz="$(basename "$YASM_URL")"

    mkdir -p "$workspace"
    cd "$workspace"
    if [[ ! -f "$tar_gz" ]]; then
        wget "$YASM_URL"
    fi
    tar xzf "$tar_gz"
    cd "${tar_gz%.tar.gz}"

    ./configure --prefix="$PREFIX" > /dev/null
    make clean > /dev/null
    make > /dev/null
    make install > /dev/null

    return 0
}

install_libfdk-aac() {
    print_head 'install libfdk-aac'

    local workspace="$TMP_DIR/libfdk-aac"

    if [[ -e "$workspace/.git" ]]; then
        cd "$workspace"
        git pull
    else
        rm -fr "$workspace"
        git clone --depth=1 --branch=master "$AAC_GIT" "$workspace"
        cd "$workspace"
    fi

    autoreconf -fiv > /dev/null
    ./configure --prefix="$PREFIX" \
        --enable-shared \
        > /dev/null
    make clean > /dev/null
    make > /dev/null
    make install-strip > /dev/null

    return 0
}

install_libx264() {
    print_head 'install libx264'

    local workspace="$TMP_DIR/libx264"

    if [[ -e "$workspace/.git" ]]; then
        cd "$workspace"
        git pull
    else
        rm -fr "$workspace"
        git clone --depth=1 --branch=master "$X264_GIT" "$workspace"
        cd "$workspace"
    fi

    ./configure --prefix="$PREFIX" \
        --enable-shared \
        --disable-ffms \
        --disable-gpac \
        --disable-lavf \
        --disable-swscale \
        --extra-cflags="$CFLAGS" \
        --extra-ldflags="$LDFLAGS" \
        > /dev/null
    make clean > /dev/null
    make > /dev/null
    make install > /dev/null

    return 0
}

install_libav() {
    print_head 'install libav'

    local workspace="$TMP_DIR/libav"

    if [[ -e "$workspace/.git" ]]; then
        cd "$workspace"
        git pull
    else
        rm -fr "$workspace"
        git clone --depth=1 --branch=master "$AV_GIT" "$workspace"
        cd "$workspace"
    fi

    ./configure --prefix="$PREFIX" \
        --extra-cflags="$CFLAGS" \
        --extra-ldflags="$LDFLAGS" \
        --enable-shared \
        --enable-nonfree \
        --enable-gpl \
        --disable-avplay \
        --disable-avserver \
        --disable-avdevice \
        --disable-network \
        --disable-devices \
        --enable-libx264 \
        --enable-libfdk-aac \
        --disable-debug \
        --disable-decoders \
        --disable-encoders \
        --disable-demuxers \
        --disable-muxers \
        --disable-protocols \
        --disable-filters \
        --enable-decoder=aac \
        --enable-decoder=ac3 \
        --enable-decoder=mp2 \
        --enable-decoder=mp3 \
        --enable-decoder=mpeg2video \
        --enable-decoder=mpeg4 \
        --enable-decoder=pcm_alaw \
        --enable-decoder=pcm_bluray \
        --enable-decoder=h264 \
        --enable-encoder=aac \
        --enable-encoder=png \
        --enable-encoder=mjpeg \
        --enable-encoder=libx264 \
        --enable-encoder=libfdk_aac \
        --enable-encoder=mpeg2video \
        --enable-encoder=mpeg4 \
        --enable-encoder=wmv2 \
        --enable-encoder=wmav2 \
        --enable-encoder=flv \
        --enable-demuxer=mpegts \
        --enable-muxer=gif \
        --enable-muxer=mjpeg \
        --enable-muxer=m4v \
        --enable-muxer=mp4 \
        --enable-muxer=mpeg2video \
        --enable-muxer=mpegts \
        --enable-muxer=flv \
        --enable-muxer=image2 \
        --enable-muxer=asf \
        --enable-muxer=asf_stream \
        --enable-protocol=pipe \
        --enable-protocol=file \
        --enable-filter=crop \
        --enable-filter=hqdn3d \
        --enable-filter=pad \
        --enable-filter=resample \
        --enable-filter=scale \
        --enable-filter=setpts \
        --enable-filter=settb \
        --enable-filter=yadif \
        --enable-filter=trim \
        > /dev/null
    make clean > /dev/null
    make > /dev/null
    make install > /dev/null

    return 0
}

main
