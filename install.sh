#!/bin/bash
# -*- coding:utf-8 -*-
# install script for setting files

link_file_list=(
    dotscreenrc
    dotemacs.d
    dotgitconfig
    dotzshrc
    dotzshrc.d
    dotzlogin
    dotbashrc
    dotrbindkeys.rb
    dotirssi
    dotxinitrc
    dotdir_colors
)

rsense_dir="$HOME/.rsense.d"

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

    local ln_opt="-sbT"
    if (echo $OSTYPE | grep "^darwin") > /dev/null
    then
        ln_opt="-sf"
    fi

    for file in ${link_file_list[@]}
    do
        local target_file="${curr_dir}/${file}"
        local dest_file="$(echo "$file" | sed -e 's/^dot/./')"
        dest_file="${HOME}/${dest_file}"

        if [ ! -e "$target_file" ]
        then
            echo "error: cannot find $target_file" >&2
            exit 1
        fi

        myln "$target_file" "$dest_file"
    done

    echo_to ~/.rbindkeys.local.rb '# -*- mode: ruby; coding: utf-8 -*-'
    create_empty_zsh ~/.zshrc.local
    create_empty_zsh ~/.zlogin.local

    setup_emacs

    # all task done?

    if [ $counter -eq 0 ]
    then
        echo "# do nothing"
    fi
    cd "$prev_dir"
}

ln_opt="-sbT"
grep -q "^darwin" <<< "$OSTYPE" \
  && ln_opt="-sf"

myln(){
    if [ -h "$dest_file" -a \
      "$target_file" = "$(readlink "$dest_file")" ]
    then return
    fi

    print_and_do "ln $ln_opt '$1' '$2'"
}

setup_emacs(){
    mkdir -p "$rsense_dir"

    if which ruby >/dev/null && which java >/dev/null
    then
	if [ ! -e "$rsense_dir/rsense-0.3" ]
	then
            local old_loc="$(pwd)"
            cd "$rsense_dir"
            print_and_do "wget http://cx4a.org/pub/rsense/rsense-0.3.tar.bz2"
            print_and_do "tar xjf rsense-0.3.tar.bz2"
            cd "$old_loc"
	fi

        [ ! -e "$HOME/.rsense" ] \
          && print_and_do "ruby ${rsense_dir}/rsense-0.3/etc/config.rb > $HOME/.rsense"
    else
        echo "WARN: cannot install rsense" 2>&1
        echo "WARN: rsense require ruby and java" 2>&1
    fi

    print_and_do "emacs --batch --eval '(setq kui/install-mode-p t)' --load '~/.emacs.d/init.el'"
}

echo_to(){
    [ ! -e $1 ] \
        && print_and_do "echo '$2' > $1"
}

create_empty_zsh(){
    echo_to "$1" '# -*- mode: sh; coding: utf-8 -*-'
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
