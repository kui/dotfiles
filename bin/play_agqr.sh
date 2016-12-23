#!/bin/bash
set -eu

# 自宅サーバでagqrを再生する
# aplay require "audio" group

if [ $# -ne 1 ]; then
  echo "Require integer at first argument"
  exit 1
fi

rtmpdump -r rtmp://fms-base1.mitene.ad.jp/agqr/aandg22 --live --stop $1 -o - | \
    ffmpeg -i - -vn -f s16le -ar 48k -ac 2 - | \
    aplay -f dat -D plughw:1,0
