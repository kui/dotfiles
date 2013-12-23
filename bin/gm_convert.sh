#!/bin/bash

USAGE="$0 interval size colors img [img [ ... ] ]

interval: <integer>
size:     <integer>x<integer>
colors:   <integer>
"

DELAY_FACTOR=3

if [[ $# -lt 4 ]]
then
    echo -e "$USAGE"
    exit
fi

interval=$1
size=$2
colors=$3
delay=$((interval * DELAY_FACTOR))

shift 3



main(){
    files=
    count=0
    for f in "$@"
    do
        [[ $((count%interval)) -eq 0 ]] && files="$files $f"
        count=$((count+1))
    done

    set -x
    gm convert -delay $delay -resize $size -colors $colors +dither $files t.gif
}

main "$@"
