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
    local workspace=$BASE_DIR/libav

    if [[ -e "$workspace/.git" ]]
    then
        cd "$workspace"
        git pull
    else
        rm -fr "$workspace"
        git clone --depth=1 --branch=master \
            "git://git.libav.org/libav.git" "$workspace"
        cd "$workspace"
    fi

    ./configure \
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
        --enable-filter=trim
    make
    make install

    return 0
}

main
