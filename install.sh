#!/bin/bash
# -*- coding:utf-8 -*-
# install script for setting files

link_file_list=(
    dotscreenrc
    dotemacs
    dotemacs.d
    dotgitconfig
    dotzshrc
    dotzshrc.d
    dotzlogin
    dotbashrc
    dotrsense
    dotrbindkeys.rb
)

if [ $OSTYPE = "cygwin" ]
then
    link_file_list=(
	dotminttyrc
	${link_file_list[@]}
    )
fi

run_flag=1
counter=0

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

    local ln_opt="-sb"
    if (echo $OSTYPE | grep "^darwin") > /dev/null
    then
	ln_opt="-sf"
    fi

    if which ruby >/dev/null && which java >/dev/null
    then
        if [ ! -e "$curr_dir/dotrsense" ]
        then
            print_and_do "ruby dotemacs.d/src/rsense-*/etc/config.rb > $curr_dir/dotrsense"
        fi
    else
        echo "WARN: cannot install rsense" 2>&1
        echo "WARN: rsense require ruby and java" 2>&1
    fi

    for file in ${link_file_list[@]}
    do
	local target_file="${curr_dir}/${file}"
	local dest_file=`echo "$file" | sed -e 's/^dot/./'`
	local dest_file="${HOME}/${dest_file}"

	if [ ! -e "$target_file" ] && (! echo "$target_file"| grep "dotrsense$" >/dev/null)
	then
	    echo "error: cannot find $target_file" >&2
	    exit 1
	fi

	# skip if $dest_file exist and the link is no change
	[ -h "$dest_file" -a\
	  "$target_file" = "`readlink "$dest_file"`" ] && continue

	print_and_do "ln $ln_opt \"$target_file\" \"$dest_file\""
    done

    create_empty_zsh ~/.zshrc.local
    create_empty_zsh ~/.zlogin.local

    # all task done?

    if [ $counter -eq 0 ]
    then
	echo "# do nothing"
    fi
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
    count
}

count(){
    counter=$((counter+1))
}

main "$@"
