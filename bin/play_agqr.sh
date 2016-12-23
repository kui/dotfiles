#!/bin/bash
set -eu

# 自宅サーバでagqrを再生する

rtmpdump -r rtmp://fms-base1.mitene.ad.jp/agqr/aandg22 --live -o - | \
    ffmpeg -i - -vn -f s16le -ar 48k -ac 2 - | \
    aplay -f dat -D plughw:1,0
