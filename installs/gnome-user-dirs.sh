#!/bin/bash
# -*- coding:utf-8 -*-
set -exu

ln_s() {
    local opts=$(
        if grep "darwin" <<< "$OSTYPE" &>/dev/null
        then echo "-sf"
        else echo "-sbT"
        fi
    )
    ln "$opts" $1 $2
}

mkdir -p "${HOME}/.config"
current="$(cd "$(dirname $0)"; pwd)"
ln_s "${current}/gnome-user-dirs.dirs" "${HOME}/.config/user-dirs.dirs"

# create user dirs
for dir in $(grep '^XDG_' "${current}/gnome-user-dirs.dirs" | cut -d'=' -f2); do
    dir="$(eval echo -e "$dir")"
    [[ ! -e "$dir" ]] && mkdir -pv "$dir"
done
