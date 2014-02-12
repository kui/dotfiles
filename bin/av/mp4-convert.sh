#!/bin/bash
set -eu

USAGE=$(cat <<EOF
usage: $(basename $0) video_file [ mp4_file_name ]
与えられた動画ファイルを mp4_file_name に変換する
EOF
)

KUI_AVCONV="/usr/local/kui-avconv"
export PATH="$KUI_AVCONV/bin:$PATH"
export LD_LIBRARY_PATH="$KUI_AVCONV/lib:$LD_LIBRARY_PATH"

if [[ $# -eq 0 ]]
then
    echo "$USAGE"
    exit 1
fi

video="$(readlink -f "$1")"
if [[ $# -ge 2 ]]
then mp4="$2"
else mp4="${video%%.*}.mp4"
fi

rm -f "$mp4"

echo "$video"
echo " => $mp4"

( avprobe "$video" 2>&1 | grep --fixed-strings 'Duration: ' ) \
    || echo "警告: 時間の抽出に失敗"
echo "現在時刻: $(date)"

# 参考:
#  https://libav.org/avconv.html
#  http://nicowiki.com/%E6%8B%A1%E5%BC%B5%20x264%20%E5%87%BA%E5%8A%9B%EF%BC%88GUI%EF%BC%89%E3%81%AE%E8%A8%AD%E5%AE%9A%E9%A0%85%E7%9B%AE%E3%81%A8%E3%81%9D%E3%81%AE%E6%A9%9F%E8%83%BD%E3%81%AB%E3%81%A4%E3%81%84%E3%81%A6.html
time avconv -i "$video" -loglevel error -threads 3 \
    -acodec libfdk_aac \
    -vcodec libx264 -s 1280x1024 \
    -profile:v main -preset:v slow -tune:v animation  \
    -filter:v yadif=2 -f mp4 \
    "$mp4"

video_size=$(stat --format=%s "$video")
mp4_size=$(stat --format=%s "$mp4")
ratio=$(bc -l <<< "100 * $mp4_size / $video_size")
printf 'Compress Ratio: %5.2f%%\n' $ratio
