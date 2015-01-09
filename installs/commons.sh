#!/bin/bash
# -*- coding: utf-8-unix -*-

is_ubuntu() {
    which lsb_release &>/dev/null && ( lsb_release -a | grep 'Ubuntu' ) &>/dev/null
}

is_mac_os_x() {
    [ "$(sw_vers -productName)" = "Mac OS X" ]
}

not() {
    ! "$@"
}

has() {
    type "$1" &>/dev/null
}

run() {
    echo_green $ "$@"
    "$@"
}
echo_green() {
    echo $'\e[32m'"${@}"$'\e[0m'
}
echo_red() {
    echo $'\e[31m'"${@}"$'\e[0m'
}
abort() {
    err "Abort: $@"
    exit 1
}
err() {
    echo_red "$@" >&2
}
warn() {
    echo_red "WARN: $@"
}
