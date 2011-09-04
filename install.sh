#!/bin/sh
# -*- coding:utf-8 -*-
# install script for setting files

file_list="
dotemacs
dotscreenrc
dotemacs.d
dotzshrc
hoge\ fuga
"

main(){
    local prev_dir=`pwd`
    local curr_dir=`dirname $0`
    cd "$curr_dir"

    for file in "$file_list"
    do
        # cmd="ln -s "
        echo $file
    done

    cd $prev_dir
}

main "$@"