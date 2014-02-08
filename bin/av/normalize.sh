#!/bin/bash
set -eu

USAGE=$(cat <<EOF
usage: $(basename $0) file.m2ts
動画ファイルを適当に圧縮する。
EOF
)

BASE_DIR="$(cd "$(dirname "$0")"; pwd)"
ARCHIVE_DIR="/var/share/tv_recorded/archives"
CHINACHU_HOME="/home/chinachu/Chinachu"
KUI_AVCONV="/usr/local/kui-avconv"

##

if [[ "$#" -ne 1 ]]; then
    echo "$USAGE"
    exit 1
fi

RECORDING_JSON_FILE="${CHINACHU_HOME}/data/recording.json"
if [[ ! -f "$RECORDING_JSON_FILE" ]]; then
    echo "録画中情報ファイルが存在しません: $RECORDING_JSON_FILE"
    exit 1
fi

export PATH="$KUI_AVCONV/bin:${PATH-}"
export LD_LIBRARY_PATH="$KUI_AVCONV/lib:${LD_LIBRARY_PATH-}"

mkdir -pv "$ARCHIVE_DIR"

main(){
    local m2ts="$(readlink -f "$1")"
    local log="${m2ts%.*}.log"
    if [[ ! -f "$m2ts" ]]; then
        echo '[スキップ]	ファイルが存在しない'
        return 0
    elif [[ -f "$log" ]]; then
        echo '[スキップ]	前回実行時にエラー有り'
        return 0
    elif is_converted "$m2ts"; then
        echo '[スキップ]	圧縮済み'
        return 0
    elif is_recording "$m2ts"; then
        echo '[スキップ]	録画中'
        return 0
    elif is_splitted "$m2ts"; then
        echo '[スキップ]	分割ファイル'
        return 0
    elif is_not_mpeg2 "$m2ts"; then
        echo '[スキップ]	not mpeg2'
        return 0
    fi

    normalize "$m2ts" | tee "$log"
    if [[ $? -eq 0 ]]; then
        rm "$log"
        echo "[成功]"
        return 0
    else
        echo "[失敗]	see $log"
        return 1
    fi
}

normalize() {
    local m2ts="$1"

    m2ts="$(rename_invalid_char "$m2ts")"

    # 前後のスポットCMをカットしたファイル生成
    echo "----- スポット CM カット -----"
    "$BASE_DIR/extract.sh" "$m2ts"
    if [[ $? -ne 0 ]]; then
        echo "----- 失敗: CM カット -----"
        return 1
    fi
    local extracted="${m2ts%.*}_extracted.${m2ts##*.}"
    if [[ ! -f "$extracted" ]]; then
        echo "期待したファイルが存在しません: ${extracted}"
        return 0
    fi

    # 圧縮・削除
    echo "----- mp4 へ圧縮 -----"
    mkdir -pv "$(dirname "${new_mp4}")"
    local mp4="$(gen_mp4_name "${extracted%_*}.dummy")"
    "$BASE_DIR/mp4-convert.sh" "${extracted}" "$mp4"
    if [[ $? -ne 0 ]]; then
        echo "----- 失敗: mp4 圧縮 -----"
        return 1
    fi
    rm -fv "${extracted}"
}

rename_invalid_char() {
    local origin="$1"
    local renamed="$1"

    renamed="$(sed 's/〜/~/g' <<< "$origin")"

    if [[ "$origin" != "$renamed" ]]; then
        mv -vf "$origin" "$renamed"
    fi
    echo "$renamed"
}

gen_mp4_name() {
    local base="$(basename "$1")"
    base="${base%.*}" # 拡張子除去

    local dir="$base"

    # Chinachu のメタデータ削除
    dir="$(perl -pe 's/\A\[.+\]//g' <<< "$dir" )"
    dir="$(perl -pe 's/\A\d{6}-\d{4}-//' <<< "$dir" )"

    echo "${ARCHIVE_DIR}/${dir}/${base}.mp4"
}

recordings="$(
    cat "$RECORDING_JSON_FILE" | \
        jq '.[] | .recorded' | \
        sed 's/^"\|"$//g' | \
        xargs -I{} basename '{}'
)"
is_recording() {
    echo "$recordings" | \
        grep --fixed-strings "$(basename "$1")" > /dev/null
}
is_not_mpeg2() {
    if avprobe "$1" >/dev/null 2>&1  | \
        grep --fixed-strings 'Video: mpeg2video' > /dev/null
    then return 1
    fi

    ! grep -qP '\.(m2)?ts\Z' <<< "$1"
}
is_splitted() {
    local file="$1"
    ext="${file##*.}"
    base="${file%_*}"
    [[ -e "$base.$ext" ]]
}
is_converted() {
    [[ -e "$(gen_mp4_name "$1")" ]]
}

main "$1"
