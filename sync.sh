#!/bin/bash
# -*- coding:utf-8-unix; mode:sh-mode; -*-
# 

task_sequence=(
    "git pull"
    # "git submodule update --recursive"
    "git submodule foreach 'git pull origin master'"
    "git push"
)

echo "num_tasks: ${#task_sequence[*]}"

i=0
while [ $i -lt ${#task_sequence[*]} ]
do
    cmd=${task_sequence[$i]}
    echo $ "$cmd"
    eval $cmd

    if [ "$?" -eq 1 ]
    then
	echo "!!!!! failed !!!!!"
	break
    fi

    i=$((i+1))
done
