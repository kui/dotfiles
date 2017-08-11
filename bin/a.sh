#!/bin/sh

set -eu

USAGE="
usage: $0 store DEST
       $0 load DEST
"

ARCHIVE="$HOME/Sync/a.tar.gz.enc"

main() {
    if [ ! $# -eq 2 ]; then
        abort "Invalid args"
    fi

    case "$1" in
        save|load)
            set -x
            "$1" "$2"
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
    if [ ! -d "$1" ]; then
        abort "Not loaded"
    fi
    cd "$1"
    time tar -cz * | openssl enc -aes-256-cbc -e > "$ARCHIVE"
}

load() {
    mkdir -vp "$1"
    time openssl enc -aes-256-cbc -d -in "$ARCHIVE" | tar -xz -C "$1"
}

browse() {
    if which xdg-open >/dev/null
    then xdg-open "$1"
    elif which start >/dev/null
    then start "$1"
    else open "$1"
    fi
}

main "$@"

