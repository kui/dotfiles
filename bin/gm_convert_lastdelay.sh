#!/bin/bash

set -eu
set -x

USAGE="$0 interval size last-delay img [img [ ... ] ]

interval: <integer>
size:     <integer>x<integer>
last-delay: <integer>
"

DELAY_FACTOR=8

if [[ $# -lt 4 ]]
then
    echo -e "$USAGE"
    exit
fi

interval=$1
size=$2
last_delay=$3
delay=$((interval * DELAY_FACTOR))

shift 3


main(){
    local files=
    local count=0
    for f in "$@"
    do count=$((count+1))
    done

    local last_index=$((count - 1))
    local count=0

    for f in "$@"
    do
        [[ $count -eq $last_index ]] && files="$files -d$last_delay"
        [[ $((count%interval)) -eq 0 ]] && files="$files $f"
        count=$((count+1))
    done

    set -x
    # gm convert -delay $delay "${files[@]}" -delay $last_delay "$last_file" s.gif
    # gm convert -resize $size -colors $colors +dither s.gif t.gif
    # gifsicle -O3 --loop --resize "$size" --colors "$colors" \
    #     -d$delay $files > ~/tmp/t.gif
    # gifsicle -O3 --loop --resize "$size" --colors "$colors" \
    #     ~/tmp/t.gif -d$delay "#0-$((last_index - 1))" -d$last_delay "#${last_index}" \
    #     > ~/tmp/s.gif
    gifsicle -O3 --loop -d$delay $files > ~/tmp/t.gif
}

main "$@"
