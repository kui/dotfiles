#!/bin/sh
# -*- coding:utf-8 -*-
# install script for setting files

main(){
    prev_dir=`pwd`
    cd `dirname $0`

    echo $1

    cd $prev_dir
}

main "$*"