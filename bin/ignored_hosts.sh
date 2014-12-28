#!/bin/bash
# -*- coding:utf-8; mode:sh; -*-

set -ue

BASE="$(cd $(dirname "$0"); pwd)"
IGNORED_HOSTS_FILE="$BASE/ignored_hosts.txt"
HOSTS_FILE='/etc/hosts'
MESSAGE="added by $BASE/$(basename "$0")"

is_dryrun=true
main(){
    if [[ -n "${1-}" && "${1-}" = "-r" ]]
    then is_dryrun=false
    else
        (
            echo '######################################################################'
            echo '## dry-run mode'
            echo '## use option `-r` if you wan to replace /etc/hosts with this result'
            echo '######################################################################'
        ) >&2
    fi

    if [[ ! -e "$IGNORED_HOSTS_FILE" ]]
    then
        echo "無視するホストファイルが見つかりません: $IGNORED_HOSTS_FILE"
        exit 1
    fi

    tmpfile="/tmp/$(head -c 18 /dev/urandom | base64 | tr '/+' '_.')"
    print_new_hosts > "$tmpfile"

    if ! $is_dryrun
    then
        cp -fv "$HOSTS_FILE" "${HOSTS_FILE}~"
        mv -fv "$tmpfile" "$HOSTS_FILE"
    else
        if type git &>/dev/null
        then git diff --word-diff /etc/hosts "$tmpfile"
        else diff /etc/hosts "$tmpfile"
        fi
        rm "$tmpfile"
    fi
}

print_new_hosts(){
    cat /etc/hosts \
        | grep -vG '^0\.0\.0\.0[^0-9]' \
        | grep -vF "## $MESSAGE ##" \
        | grep -vF "##/$MESSAGE ##"

    echo "## $MESSAGE ##"
    echo -n '0.0.0.0 '
    grep -v '^#' "$IGNORED_HOSTS_FILE" | tr '\n' ' '
    echo
    echo "##/$MESSAGE ##"
}

main "$@"
