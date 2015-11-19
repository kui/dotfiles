#!/bin/bash
# -*- coding: utf-8-unix -*-
set -eu

BASE="$(cd "$(dirname $0)"; pwd)"
. "${BASE}/commons.sh"

run git clone --depth 1 https://github.com/junegunn/fzf.git "$HOME/.fzf"
run "$HOME"/.fzf/install --key-bindings --completion --no-update-rc
