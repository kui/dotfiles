#!/bin/bash
# -*- coding:utf-8 -*-
set -eu

MACPORTS_INSTALLS=(
    git zsh curl wget coreutils findutils xz ctags gsed debianutils
)
HOMEBREW_INSTALLS=(
    git zsh curl wget coreutils findutils xz ctags gnu-sed debianutils
)
UBUNTU_INSTALLS=(
    git zsh curl wget ssh build-essential xz-utils exuberant-ctags
)
BASE_DIR="$HOME/.dotfiles"
LN=
if which gln &>/dev/null
then LN=gln
else LN=ln
fi

main() {
    install_basics
    if [[ -e "$BASE_DIR" ]];
    then cd "$BASE_DIR"; run git push origin master
    else run git clone git@github.com:kui/dotfiles.git "$BASE_DIR"
    fi
    cd "$BASE_DIR"
    run pwd

    run git submodule init
    run git submodule update

    install_dotfiles
    install_templates

    echo_green "Success!!"
    echo_green "Next, see the following `installs/` scripts: "
    find installs -name '*.sh' | column
}

install_basics() {
    if which lsb_release &>/dev/null && (lsb_release -a | grep 'Ubuntu') &>/dev/null; then
        run sudo apt-get install -y "${UBUNTU_INSTALLS[@]}"
    elif grep "darwin" <<< "$OSTYPE" &>/dev/null; then
        if which brew &>/dev/null; then
            run brew install "${HOMEBREW_INSTALLS[@]}"
        elif which port &>/dev/null; then
            run sudo port install "${MACPORTS_INSTALLS[@]}"
        else
            abort "Require MacPorts or HomeBrew"
        fi
    else
        abort "Non supported platform"
    fi
}

install_dotfiles() {
    local file
    for file in $(pwd)/dotfiles/*; do
        local dest="$HOME/.$(basename $file)"
        run $LN -sbT "$file" "$dest"
    done
}

install_templates() {
    local file
    for file in templates/*; do
        local dest="$HOME/.$(basename $file)"
        if [[ -e "$dest" ]]; then
            warn "Skip the template installation: Already exist $file"
            continue
        fi
        run cp "$file" "$dest"
    done
}

run() {
    echo_green $ "$@"
    env "$@"
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

main
