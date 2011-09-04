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

    while getopts r:run: opt
    do
    done

    for file in $file_list
    do
        local dest_file=`echo "$file" | sed -e 's/^dot/./'`
        cmd="ln -s ${curr_dir}/${file} ${HOME}/${dest_file}"
        echo $cmd
        # $cmd
    done

    cd $prev_dir
}

main "$@"