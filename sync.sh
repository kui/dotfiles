#!/bin/bash
# -*- coding:utf-8-unix; mode:sh-mode; -*-
# 

task_sequence=(
    "git pull"
    "git submodule update --init --recursive"
    "git push"
)

echo "num_tasks: ${#task_sequence[*]}"

i=0
while [ $i -lt ${#task_sequence[*]} ]
do
    cmd=${task_sequence[$i]}
    echo $ "$cmd"
    $cmd

    if [ "$?" -eq 1 ]
    then
	echo "!!!!! failed !!!!!"
	break
    fi

    i=$((i+1))
done
