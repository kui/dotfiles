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
    while getopts r opt
    do
        case $opt in
            r) local run_flag=0 ;;
        esac
    done

    if [ $run_flag -eq 1 ]
    then
        echo "##################################################################"
        echo "# dry-run. if you want to run these commands, attache -r option. #"
        echo "##################################################################"
    fi

    for file in $file_list
    do
        local target_file="${curr_dir}/${file}"
        local dest_file=`echo "$file" | sed -e 's/^dot/./'`
        local dest_file="${HOME}/${dest_file}"
        local cmd="";
        if [ -e $dest_file ]
        then
            local cmd="mv \"$dest_file\" \"${dest_file}.old\";"
        fi

        local cmd="${cmd}ln -s \"$target_file\" \"$dest_file\""
        echo "run \`$cmd\`"
        if [ $run_flag -eq 0 ]
        then
            eval "$cmd"
        fi
    done

    cd $prev_dir
}

main "$@"