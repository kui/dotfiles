# -*- mode: sh; coding: utf-8-unix -*-

execute_if_exists(){
    local file="$1"
    if [ -e $file ]
    then
        echo "# run $file"
    fi
}

execute_if_exists "~/.settings.d/.syncsyncgit.sh"