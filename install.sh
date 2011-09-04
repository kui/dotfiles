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

    local run_flag=1
    while getopts r:run: opt
    do
        case $opt in
            r | run) local run_flag=0 ;;
        esac
    done

    if [ run_flag -eq 1 ]
    then
        echo "# dry-run. if you want to run these commands, attache -r option."
    fi

    for file in $file_list
    do
        local dest_file=`echo "$file" | sed -e 's/^dot/./'`
        cmd="ln -s ${curr_dir}/${file} ${HOME}/${dest_file}"
        echo $cmd
        if [ run_flag -eq 1 ]
        then
        #$cmd
        fi
    done

    cd $prev_dir
}

main "$@"