#!/bin/bash
# -*- coding:utf-8; mode:sh; -*-

set -ue

BASE="$(cd $(dirname "$0"); pwd)"
IGNORED_HOSTS_FILE="$BASE/ignored_hosts.txt"
HOSTS_FILE='/etc/hosts'

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

    tmpfile="$(tempfile --mode 0644)"
    print_new_hosts > "$tmpfile"

    if ! $is_dryrun
    then
        mv --force --verbose --backup "$tmpfile" "$HOSTS_FILE"
    else
        diff /etc/hosts "$tmpfile"
    fi
}

print_new_hosts(){
    grep -vP '^0\.0\.0\.0[^0-9]' /etc/hosts
    echo -n '0.0.0.0 '
    grep -v '^#' "$IGNORED_HOSTS_FILE" | tr '\n' ' '
    echo
}

main "$@"
