#!/bin/sh
#
# syncsyncbox.sh
#  use a git repository
#

INTERVAL="60"

GC_INTERVAL=20

main(){

    case $1 in
        start) run ;;
        stop) stop ;;
    esac

}

run(){

    if ! is_git_dir
    then
        echo "the current dir is not a git ripository" >&2
        exit 1
    fi

    echo "start sync (pid: $$)" | logger
    count=0
    while true
    do
        sync | logger
        if [ $count -gt $GC_INTERVAL ]
        then
            git gc 2>&1 | logger
            # echo "git gc" | logger
            count=0
        fi
        sleep $INTERVAL
        count=$[$count+1]
    done
}

is_git_dir(){
    git status > /dev/null 2>&1
}

logger(){
    local datetime=`date +'%F %T'`
    sed -e "s/^/$datetime /" $1
}

sigint_hook(){
    echo 
    echo "exit $0"
    exit 0
}

sync(){
    git add . 2>&1
    local dry_run=`commit --porcelain 2>&1`
    if [ -n "$dry_run" ]
    then
        echo $dry_run
        commit --quiet
    fi
    git push --quiet 2>&1 | grep -v "^Everything up-to-date$"
    git pull --ff 2>&1 | grep -v "^Already up-to-date.$"
}

commit(){
    options="$*"
    git commit --all --message "`date +'%F %T'` $0" $options 2>&1 |\
      grep -v "^# On branch master$" |\
      grep -v "^nothing to commit (working directory clean)$"
 }
 
main "$@"
