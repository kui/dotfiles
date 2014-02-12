#!/bin/bash
set -eu

export PATH=/usr/sbin:/sbin:/bin:/usr/bin:/usr/local/bin:"$PATH"

BASE_DIR="$(dirname "$(readlink -f "$0")")"
REC_DIR=/var/share/tv_recorded
LOG="$REC_DIR/archives/log/archive.log"

main() {
    local f
    echo "--------- 開始: $(date) ----------"
    for f in "$REC_DIR"/{,**/}*.{,m2}ts; do
        [[ ! -f "$f" ]] && continue
        echo "## $f"
        "$BASE_DIR/delete-if-older.sh" "$f"
        [[ ! -f "$f" ]] && continue
        "$BASE_DIR/normalize.sh" "$f"
    done
    echo "--------- 終了: $(date) ----------"
}

lr_conf="$(tempfile)"
cat <<EOF > "$lr_conf"
"$LOG" {
  monthly
  rotate 4
  compress
  nomail
  missingok
}
EOF

mkdir -p "$(dirname "$LOG")"
logrotate --state "$REC_DIR/.logrotate.status" "$lr_conf"
if [[ -n "${DEBUG-}" ]]
then main
else main >> "$LOG" 2>&1
fi
