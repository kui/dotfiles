#!/bin/bash

set -x

BASE="$(dirname $0)"

TSSPLITTER="${BASE}/wine/TsSplitter.exe"
env WINEARCH=win32 WINEPREFIX=~/.wine32 \
    wine "$TSSPLITTER" -SEP2 -LOG -EIT -ECM -EMM -1SEG "$@"
