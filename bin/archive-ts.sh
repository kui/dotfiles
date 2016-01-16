#!/bin/bash
# -*- coding:utf-8-unix; mode:sh; -*-

set -u

cd $(dirname "$0")

export PATH=/usr/local/bin:/usr/bin:/bin

BASE_DIR="/var/share/tv_recorded"
TMP="${TMP-/tmp/av-archive}"
SPLIT="./split-ts.sh"
CONVERT="./convert-mp4.sh"

main() {
    local m2ts
    for m2ts in "$BASE_DIR"{,/**}/*.m2ts; do
        if [[ ! -e "$m2ts" ]]
        then continue
        fi

        local new_m2ts="$(normalize_file_name "$m2ts")"
        if [[ "$m2ts" != "$new_m2ts" ]]; then
            mv -v "$m2ts" "$new_m2ts"
            m2ts="$new_m2ts"
        fi

        local mp4="$(get_mp4_name "$m2ts")"
        local log="$(get_log_name "$m2ts")"

        if [[ -e "$mp4" ]]
        then continue
        fi

        if [[ -e "$log" ]]
        then continue
        fi

        archive "$m2ts" 2>&1 | tee -a "$log"
        if [[ ${PIPESTATUS[0]} -ne 0 ]]
        then break
        fi
        rm "$log"
    done

    rm -frv "$TMP"
}

normalize_file_name() {
    sed 's/〜/~/g' <<<"$*"
}

archive() {
    echo "##"
    echo "## archive $1"
    echo "##"

    local m2ts="$1"
    local mp4="$(get_mp4_name "$m2ts")"
    local log="$(get_log_name "$m2ts")"
    local tmpdir="$TMP/$(head -c 7 /dev/urandom | xxd -p)"

    mkdir -pv "$tmpdir"

    # wait some micro secs each file read
    time "$SPLIT" -OUT "$tmpdir" -WAIT,3 "$m2ts"

    ls -S "$tmpdir"/* | tail -n +2 | xargs rm -fv
    local extracted="$(ls -S "$tmpdir"/* | head -n1)"
    time "$CONVERT" "$extracted" "$mp4"

    rm -frv "$tmpdir"
}

get_mp4_name() {
    printf "$(prefix "$1").mp4"
}

get_log_name() {
    printf "$(prefix "$1").log"
}

prefix() {
    printf "${1%.*}"
}

main
