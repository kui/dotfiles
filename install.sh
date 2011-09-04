#!/bin/sh
# -*- coding:utf-8 -*-
# install script for setting files

files="
dotemacs
dotscreenrc
dotemacs.d
dotzshrc
"

main(){
    local prev_dir=`pwd`
    local curr_dir=`dirname $0`
    cd "$curr_dir"

    for file in $files
    do
        cmd="ln -s "
    done

    cd $prev_dir
}

main "$@"