#!/bin/bash
# -*- coding:utf-8; mode:sh; -*-

USAGE="usage: $(basename $0) video_file start_time duration start_frame_num end_frame_num interval last_delay
tumblr にアップロードするための gif 画像を作成する。
画像の大きさを調節して、ファイルサイズを適切なサイズにする。"

MAX_WIDTH=500
MIN_WIDTH=250
MAX_GIF_SIZE=$((900 * 1000)) # Bi
MIN_GIF_SIZE=$((920 * 1000)) # Bi
BASE_DIR=/home/kui/tmp/tumblr-gif
FRAME_GIFS="${BASE_DIR}/t"
OUTPUT_GIF="${BASE_DIR}/t.gif"
DELAY_FACTOR=7

if [ $# -ne 7 ]
then
    echo "$USAGE"
    exit 1
fi

set -eu
set -x

video_file=$1
start_time=$2
duration=$3
start_frame_num=$4
end_frame_num=$5
interval=$6
last_delay=$7

shift 7

delay=$((interval * DELAY_FACTOR))
upper_width=$MAX_WIDTH
lower_width=$MIN_WIDTH

main(){
    init

    local w=$MAX_WIDTH
    while :
    do
        local h=$(printf %d $((w * 280 / 500))) # 500x280 を基準に高さを導出
        generate_gifs "${w}x${h}"
        move_window $w

        local nw=$(next_width $w)
        if [ $nw -eq $w ]
        then
            echo "gif 作成成功しました"
            ls $OUTPUT_GIF
            xdg-open $OUTPUT_GIF
            exit 0
        fi
        if ! validate_width $nw
        then
            echo "リサイズ無理でした"
            exit 1
        fi

        w=$nw
    done
}

init(){
    make_all_dirs
}

make_all_dirs(){
    rm -fr "$BASE_DIR" "$FRAME_GIFS" "$OUTPUT_GIF"
    mkdir -p $BASE_DIR $FRAME_GIFS $(dirname $OUTPUT_GIF)
}

generate_gifs(){
    echo generate gif: $1

    avconv -ss "$start_time" -an -deinterlace -loglevel panic \
        -i "$video_file" -r 3 -s "$1" -t "$duration" "$FRAME_GIFS/%04d.gif"

    local files=$(seq -f "$FRAME_GIFS/%04g.gif" $start_frame_num $end_frame_num)
    local args="-d$delay"
    local count=0
    local files_num=$(echo "$files" | wc -l)
    local last_index=$((files_num - 1))
    for f in $files
    do
        if [ $count -eq $last_index ]
        then args="$args -d$last_delay $f"
        elif [ $((count % interval)) -eq 0 ]
        then args="$args $f"
        fi
        count=$((count + 1))
    done

    gifsicle -O3 --loop $args > $OUTPUT_GIF
}

validate_size(){
    local size=$(stat -c%s $OUTPUT_GIF)
    [ -f $OUTPUT_GIF \
        -a $MIN_GIF_SIZE -lt $size \
        -a $size -lt $MAX_GIF_SIZE ]
}

move_window(){
    local current=$1
    local size=$(stat -c%s $OUTPUT_GIF)
    if [ $size -lt $MIN_GIF_SIZE ] # 小さすぎ
    then lower_width=$current
    elif [ $MAX_GIF_SIZE -lt $size ] # 大きすぎ
    then upper_width=$current
    fi
}

next_width(){
    local current=$1
    local size=$(stat -c%s $OUTPUT_GIF)
    if [ $((upper_width - lower_width)) -lt 10 ] \
        || [ $MIN_GIF_SIZE -lt $size -a $size -lt $MIN_GIF_SIZE ]
    then echo $current
    else echo $(((lower_width + upper_width) / 2))
    fi
}

validate_width(){
    local w=$1
    [ $((MIN_WIDTH + 10)) -lt $w ]
}

main
