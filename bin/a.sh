#!/bin/sh

set -eu

USAGE="
usage: $0 store
       $0 load
"

DIR='zaqwsx'

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
    (
        printf "$@"
        printf "\n"
        printf "$USAGE"
    ) >&2
    exit 1
}

save() {
    if [ ! -d "/tmp/$DIR" ]; then
        abort "Not loaded"
    fi
    time tar -C /tmp -cz "$DIR" | gpg --yes -cao ~/Sync/a
}

load() {
    gpg -o- --decrypt ~/Sync/a | tar -C /tmp -xz && browse "/tmp/$DIR"
}

browse() {
    if which xdg-open >/dev/null
    then xdg-open "$1"
    else open "$1"
    fi
}

main "$@"
