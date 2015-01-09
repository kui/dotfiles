#!/bin/bash
# -*- coding:utf-8 -*-
set -exu

current="$(cd "$(dirname $0)"; pwd)"
ln -sbT "${current}/eclipse.desktop" "$HOME/.local/share/applications/eclipse.desktop"
