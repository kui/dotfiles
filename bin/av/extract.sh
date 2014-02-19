#!/bin/bash
set -eu

USAGE=$(cat <<EOF
usage: $(basename $0) file.m2ts
与えられたファイルの番組情報を元にファイル分割し、
一番それらしい分割ファイルを残して削除する
EOF
)

BASE="$(cd "$(dirname "$0")"; pwd)"
SUFFIX="${SUFFIX-_extracted}"

if [[ "$#" -eq 0 ]]; then
    echo "$USAGE"
    exit 1
fi

main() {
    local m2ts="$1"
    local base="${m2ts%.*}"
    local ext="${m2ts##*.}"
    local extracted="${base}${SUFFIX}.${ext}"
    echo "カット後ファイル: $extracted"

    "$BASE/split.sh" "$m2ts"

    local splitteds_num=$(ls --size "${base}"_*."${ext}" | \
        grep --invert-match --fixed-strings "$m2ts" | \
        wc -l)
    if [[ $splitteds_num -eq 0 ]]; then
        echo "エラー: 分割したはずのファイルが見つかりません" >&2
        return 1
    fi

    local max_splitted="$(
        ls --size "${base}"_*."${ext}" | \
            grep --invert-match --fixed-strings "$m2ts" | \
            grep --invert-match --fixed-strings "$extracted" | \
            sort --reverse | \
            awk '{ print $2 }' | \
            head --lines=1)"
    mv -fv "$max_splitted" "$extracted"

    # 不要な分割ファイルを削除
    ls "${base}"_*."${ext}" | \
        grep --invert-match --fixed-strings "$m2ts" | \
        grep --invert-match --fixed-strings "$extracted" | \
        xargs -I{} rm -v '{}'
}

main "$1"
