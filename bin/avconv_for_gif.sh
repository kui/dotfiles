#!/bin/bash

set -exu

USAGE="$(basename $0) video_file width offset_time length"


if [ $# -ne 4 ]
then
    echo -e "$USAGE"
    exit
fi

width=$2
height=$((width * 280 / 500)) # "500x280" を基準に算出
size="${width}x${height}"

[ -e ~/tmp/t ] && rm -fr ~/tmp/t
mkdir -p ~/tmp/t
avconv -ss "$3" -an -deinterlace -i "$1" -r 3 -s "$size" -t "$4" ~/tmp/t/%04d.gif
