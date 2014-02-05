#!/bin/bash
set -eu

USAGE=$(cat <<EOF
usage: $(basename $0) video_file [ video_file ... ]
与えられた動画ファイルを適当な mp4 に変換する。
EOF
)

if [[ $# -eq 0 ]]
then
    echo "$USAGE"
    exit 1
fi

for video in "$@"
do
    local video="$(readlink -f "$video")"
    local mp4="${video%%.*}.mp4"

    set -x
    avconv -i "$video" -loglevel error -y -threads 0 -coder ac -flags +loop+ilme \
        -cmp +chroma -partitions +parti8x8+parti4x4+partp8x8+partb8x8 \
        -me_method umh -me_range 16 -subq 8 -g 50 -keyint_min 25 -sc_threshold 40 \
        -i_qfactor 0.71 -b_strategy 2 -qcomp 0.6 -qmin 10 -qdiff 4 -bf 4 \
        -refs 4 -direct-pred 3 -trellis 1 -f mp4 -vcodec libx264 \
        -flags2 +bpyramid+wpred+mixed_refs+dct8x8+fastpskip \
        -deinterlace -bt 3M -vsync 1 -aspect 16:9 \
        -ac 2 -ar 48000 -ab 128k -map 0:0 -map 0:1 \
        "$mp4"
    set +x
done

## 参考にしたURL
# transitive.info - TS から H.264 の MPEG4 にする
# http://transitive.info/2013/01/02/ts-compression-mpeg4-h264-aac/
# http://memo.hoyo.asia/index.php?%E3%82%A8%E3%83%B3%E3%82%B3%E3%83%BC%E3%83%89%E8%A8%AD%E5%AE%9A%2F%E3%82%A2%E3%83%8B%E3%83%A1
