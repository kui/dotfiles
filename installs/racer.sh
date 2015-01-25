#!/bin/bash
# -*- coding:utf-8 -*-
set -exu

current="$(cd "$(dirname $0)"; pwd)"

if [[ -e "$HOME/.local/rust" ]]
then cd "$HOME/.local/rust" && git pull origin master
else git clone git@github.com:rust-lang/rust.git "$HOME/.local/rust"
fi

if [[ -e "$HOME/.local/racer" ]]
then cd "$HOME/.local/racer" && git pull origin master
else git clone git@github.com:phildawes/racer.git "$HOME/.local/racer"
fi

cd "$HOME/.local/racer"
cargo build

export RUST_SRC_PATH="$HOME/.local/rust/src"
