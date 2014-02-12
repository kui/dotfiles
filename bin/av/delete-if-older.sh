#!/bin/bash
set -e

USAGE=$(cat <<EOF
usage: $(basename $0) file.m2ts
与えられたファイルの中から古めの動画ファイルを削除する
EOF
)

# 秒で指定
THRESHOLD=$((7 * 24 * 60 * 60)) # 1 週間

##

if [[ "$#" -ne 1 ]]; then
    echo "$USAGE"
    exit 1
fi

BASE="$(dirname "$(readlink -f "$0")")"
CURRENT=$(date +%s)
OLDER=$((CURRENT - THRESHOLD))

main() {
    local m2ts="$1"
    if [[ -f "$m2ts" ]] && \
        is_ts "$m2ts" && \
        is_older "$m2ts"
    then rm -fv "$m2ts"
    fi
}

is_ts() {
    grep -qE '\.(m2)?ts$' <<< "$1"
}
is_older() {
    [[ $OLDER -gt $(stat -c%Y "$1") ]]
}

main "$1"
