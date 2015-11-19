#!/bin/bash
# -*- coding:utf-8 -*-
set -eu

HOMEBREW_INSTALLS=(
    git zsh curl wget coreutils findutils xz ctags gnu-sed debianutils
    terminal-notifier reattach-to-user-namespace
)
UBUNTU_INSTALLS=(
    git zsh curl wget ssh build-essential xz-utils exuberant-ctags
)
BASE_DIR="$HOME/.dotfiles"

main() {
    install_basics

    git_clone_or_pull git@github.com:kui/dotfiles.git "$BASE_DIR"

    cd "$BASE_DIR"
    run pwd

    run git submodule init
    run git submodule update

    install_dotfiles
    install_templates

    echo_green "Success!!"
    echo_green "Next, see the following 'installs/' scripts: "
    find installs -name '*.sh' | column
}

install_basics() {
    if which lsb_release &>/dev/null && (lsb_release -a | grep 'Ubuntu') &>/dev/null; then
        run sudo apt-get install -y "${UBUNTU_INSTALLS[@]}"
    elif grep "darwin" <<< "$OSTYPE" &>/dev/null; then
        if [[ ! -e ~/.homebrew ]]; then
            run mkdir ~/.homebrew
            run curl -L https://github.com/Homebrew/homebrew/tarball/master | tar xz --strip 1 -C ~/.homebrew
        fi
        PATH=$HOME/.homebrew/bin:$PATH
        run $HOME/.homebrew/bin/brew install "${HOMEBREW_INSTALLS[@]}"
    else
        abort "Non supported platform"
    fi
}

git_clone_or_pull() {
    local git_uri=$1
    local dir=$2
    if [[ -e "$dir" ]]; then
        local pwd=$(pwd)
        cd "$dir"
        run git pull
        cd "$pwd"
    else
        run git clone "$git_uri" "$dir"
    fi
}

install_dotfiles() {
    if which gln &>/dev/null
    then LN=gln
    else LN=ln
    fi

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
