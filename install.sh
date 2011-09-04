#!/bin/sh
# -*- coding:utf-8 -*-
# install script for setting files

file_list="
dotemacs
dotscreenrc
dotemacs.d
dotzshrc
"

main(){
    local prev_dir=`pwd`
    local curr_dir=`dirname $0`
    cd "$curr_dir"
    local curr_dir=`pwd`

    for file in "$file_list"
    do
        local dest_file=``
        cmd="ln -s ${curr_dir}/${file} $HOME"
        echo $cmd
        # $cmd
    done

    cd $prev_dir
}

main "$@"