# -*- mode: sh; coding: utf-8-unix -*-

file_list="
~/.settings.d/.syncsyncgit.sh
"

for file in $file_list
    if [ -e $file ]
    then
        echo "# run $file"
        eval "$file"
    fi
}
