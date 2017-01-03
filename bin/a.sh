#!/bin/sh

set -eu

USAGE="
usage: $0 store
       $0 load
"

BASE="/tmp/zaqwsx"
DEST="$HOME/Sync/a.tar.gz.enc"

main() {
    if [ ! $# -eq 1 ]; then
        abort "Invalid args"
    fi

    case "$1" in
        save|load)
            set -x
            $1
            ;;
        *)
            abort "Unknown command: $1"
            ;;
    esac
}

abort() {
    ( printf "$@"
      printf "\n"
      printf "$USAGE"
    ) >&2
    exit 1
}

save() {
    if [ ! -d "$BASE" ]; then
        abort "Not loaded"
    fi
    cd "$BASE"
    time tar -cz * | openssl enc -aes-256-cbc -e > "$DEST"
}

load() {
    mkdir -vp "$BASE"
    if ! mount | grep -qF "$BASE"; then
        sudo mount -v -t tmpfs -o size=512m tmpfs "$BASE"
    fi
    time openssl enc -aes-256-cbc -d -in "$DEST" | tar -xz -C "$BASE"
}

browse() {
    if which xdg-open >/dev/null
    then xdg-open "$1"
    else open "$1"
    fi
}

main "$@"
