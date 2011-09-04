#!/bin/sh
# -*- coding:utf-8 -*-
# install script for setting files

files=(
    dotemacs
    dotscreenrc
    dotemacs.d
    dotzshrc
)

main(){
    local prev_dir=`pwd`
    cd `dirname $0`

    for file in $files
    do
        echo $file
    done

    cd $prev_dir
}

main "$@"