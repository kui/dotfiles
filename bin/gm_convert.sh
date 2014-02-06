#!/bin/bash

set -eu
set -x

USAGE="$0 interval size img [img [ ... ] ]

interval: <integer>
size:     <integer>x<integer>
"

DELAY_FACTOR=8

if [[ $# -lt 3 ]]
then
    echo -e "$USAGE"
    exit
fi

interval=$1
size=$2
delay=$((interval * DELAY_FACTOR))

shift 2

main(){
    files=
    count=0
    for f in "$@"
    do
        [[ $((count%interval)) -eq 0 ]] && files="$files $f"
        count=$((count+1))
    done

    set -x
    # gm convert -delay $delay -resize $size -colors $colors +dither $files t.gif
    gifsicle -O3 --delay "$delay" --loop --resize "$size" $files > ~/tmp/t.gif
    echo see ~/tmp/t.gif
}

main "$@"
