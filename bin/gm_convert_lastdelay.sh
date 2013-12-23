#!/bin/bash

USAGE="$0 interval size colors last-delay img [img [ ... ] ]

interval: <integer>
size:     <integer>x<integer>
colors:   <integer>
last-delay: <integer>
"

DELAY_FACTOR=3

if [[ $# -lt 5 ]]
then
    echo -e "$USAGE"
    exit
fi

interval=$1
size=$2
colors=$3
last_delay=$4
delay=$((interval * DELAY_FACTOR))

shift 4



main(){
    local files=()
    local count=0
    for f in "$@"
    do
        [[ $((count%interval)) -eq 0 ]] && files=(${files[@]} "$f")
        count=$((count+1))
    done

    local last_index=$((${#files[@]} - 1))
    local last_file=${files[last_index]}

    # remove the last file
    unset files[last_index]
    files=(${files[@]})

    set -x
    gm convert -delay $delay "${files[@]}" -delay $last_delay "$last_file" s.gif
    gm convert -resize $size -colors $colors +dither s.gif t.gif
}

main "$@"
