#!/bin/bash

set -eu

DEV_NAME=ThinkPad
LOG=~/.local/log/rbindkeys.log

##

# debug
#set -x

if [[ $EUID -ne 0 ]]
then
    echo "ERROR: this script must be run as root" 1>&1
    exit 1
fi

export RBENV_ROOT="/usr/local/rbenv"
export PATH="/usr/local/rbenv/bin:$PATH"

main(){
    echo '##############################################'
    date
    echo "Display: $DISPLAY"

    eval "$(rbenv init -)"
    rbenv shell 1.9.3-p484

    dev="$(rbindkeys -l | grep -F "$DEV_NAME" | cut -d: -f1 | head -n 1)"
    echo "Device: $dev"

    if lsof "$dev" | cut -d' ' -f1 | grep -qP '^rbindkeys'
    then
        echo "ERROR: '$dev' has already be watched by rbindkeys"
        exit 1
    fi

    rbindkeys "$dev" > /dev/null
}

mkdir -p "$(dirname "$LOG")"
main | tee -a "$LOG"
