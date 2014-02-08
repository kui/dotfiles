#!/bin/bash -eu

if [[ "$#" -eq 0 ]]; then
    exit 1
fi

BASE="$(cd "$(dirname "$0")"; pwd)"
TSSPLITTER="${BASE}/../wine/TsSplitter.exe"

env WINEARCH=win32 WINEPREFIX=~/.wine32 \
    wine "$TSSPLITTER" -SEP2 -EIT -ECM -EMM -1SEG "$@" | nkf -Sw

exit ${PIPESTATUS[0]}
