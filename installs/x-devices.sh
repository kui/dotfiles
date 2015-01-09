#!/bin/bash
# -*- coding:utf-8 -*-
set -eux

if [[ $EUID -ne 0 ]]; then
    echo "ERROR: This script must be run as root"
    exit 1
fi

base="$(cd "$(dirname "$0")"; pwd)"
mkdir -pv /etc/X11/xorg.conf.d
ln -sbT "${base}/x-devices.conf" /etc/X11/xorg.conf.d/99-kui-devices.conf
