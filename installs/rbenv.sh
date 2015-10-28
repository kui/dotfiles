#!/bin/bash
# -*- coding: utf-8-unix -*-
set -eux

_git() {
    uri="$1"
    dest="$2"

    if [[ -d "$dest" ]]; then
        pushd "$dest"
        git pull
        popd
    elif [[ -e "$dest" ]]; then
        echo "すでに存在します: $dest" >&2
        exit 1
    else
        git clone "$uri" "$dest"
    fi
}

_git https://github.com/sstephenson/rbenv.git ~/.rbenv
_git https://github.com/sstephenson/ruby-build.git \
    ~/.rbenv/plugins/ruby-build
_git https://github.com/tpope/rbenv-ctags.git ~/.rbenv/plugins/rbenv-ctags
_git https://github.com/sstephenson/rbenv-default-gems.git \
    ~/.rbenv/plugins/rbenv-default-gems

if [[ ! -f ~/.rbenv/default-gems ]]; then
    cat > ~/.rbenv/default-gems <<EOF
bundler
rake
pry
pry-doc >=0.6.0
method_source >=0.8.2
EOF
fi

# https://github.com/sstephenson/ruby-build/wiki
if which apt-get &> /dev/null; then
    sudo apt-get install autoconf bison build-essential libssl-dev \
        libyaml-dev libreadline6 libreadline6-dev zlib1g zlib1g-dev
elif which yum &> /dev/null; then
    sudo yum install gcc-c++ glibc-headers openssl-devel readline \
        libyaml-devel readline-devel zlib zlib-devel
elif which brew &> /dev/null; then
    brew install openssl libyaml pkg-config autoconf
elif which port &> /dev/null; then
    port install openssl libyaml
else
    echo "Ruby をビルドするためのパッケージをインストールしてください"
fi
