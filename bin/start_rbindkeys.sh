#!/bin/bash

set -eu

LOG=$HOME/.local/log/rbindkeys.log
DEV_NAME='ThinkPad USB Keyboard|AT Translated Set 2 keyboard|Topre Corporation Realforce 87'

##

# debug
#set -x

if [[ $EUID -ne 0 ]]
then
    echo "ERROR: this script must be run as root" 1>&1
    exit 1
fi

main(){
    echo '##############################################'
    date
    echo "Display: $DISPLAY"

    dev="$(rbindkeys -l | grep -E "$DEV_NAME" | cut -d: -f1 | head -n 1)"
    echo "Device: $dev"

    if lsof "$dev" | cut -d' ' -f1 | grep -qP '^rbindkeys'
    then
        echo "ERROR: '$dev' has already be watched by rbindkeys"
        exit 1
    fi

    rbindkeys "$dev" > /dev/null
}

mkdir -p "$(dirname "$LOG")"
main 2>&1 | tee -a "$LOG"
