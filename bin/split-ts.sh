#!/bin/bash

set -eu

BASE="$(cd "$(dirname "$0")"; pwd)"
TSSPLITTER="${BASE}/wine/TsSplitter.exe"

env WINEARCH=win32 WINEPREFIX=~/.wine32 \
    wine "$TSSPLITTER" -SEP -EIT -ECM -EMM -1SEG "$@" | nkf -u -Sw

exit ${PIPESTATUS[0]}
