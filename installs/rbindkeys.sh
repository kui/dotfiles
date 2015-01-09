#!/bin/bash
# -*- coding:utf-8 -*-
set -exu

sudo gem install rbindkeys

current="$(cd "$(dirname $0)"; pwd)"
ln -sbT "${current}/rbindkeys.desktop" /home/kui/.config/autostart/rbindkeys.desktop
