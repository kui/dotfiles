#!/bin/sh
# -*- coding:utf-8 -*-
# install script for setting files

link_file_list="
dotscreenrc
dotemacs
dotemacs.d
dotzshrc
dotzlogin
"

run_flag=1

main(){
    local prev_dir=`pwd`
    local curr_dir=`dirname $0`
    cd "$curr_dir"
    local curr_dir=`pwd`

    run_flag=1
    while getopts r opt
    do
        case $opt in
            r) run_flag=0 ;;
        esac
    done

    if [ $run_flag -eq 1 ]
    then
        echo "##############################################################"
        echo "# dry-run (if these commands are correct, attach -r option.) #"
        echo "##############################################################"
    fi

    for file in $link_file_list
    do
        local target_file="${curr_dir}/${file}"
        local dest_file=`echo "$file" | sed -e 's/^dot/./'`
        local dest_file="${HOME}/${dest_file}"

        if ! [ -e "$target_file" ]
        then
            echo "error: cannot find $target_file" >&2
            exit 1
        fi

        if [ -e "$dest_file" ]
        then
            [ "`echo "$target_file"`" = "`readlink "$dest_file"`" ] && continue
            print_and_do "mv \"$dest_file\" \"${dest_file}.old\""
        fi

        print_and_do "ln -s \"$target_file\" \"$dest_file\""
    done


    create_empty_zsh ~/.zshrc.local
    create_empty_zsh ~/.zlogin.local

    cd "$prev_dir"
}

create_empty_zsh(){
    if ! [ -e $1 ]
    then
        print_and_do "echo \"# -*- mode: sh; coding: utf-8 -*-\" > $1"
    fi
}

print_and_do(){
    local cmd="$1"
    echo "$cmd"
    [ $run_flag -eq 0 ] && eval "$cmd"
}

main "$@"
