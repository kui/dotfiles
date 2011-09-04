#!/bin/sh
#
# syncsyncbox.sh
#  use a git repository
#

INTERVAL="60"

GC_INTERVAL=20

main(){

    cd `dirname $0`

    echo `get_pid_file_name`
    case $1 in
        start) run ;;
        stop) stop ;;
        sync) sync ;;
        *) help;;
    esac

}

run(){

    create_pid_file

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

stop(){
    delete_pid_file
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

create_pid_file(){
    echo `get_pid_file_name`
}

delete_pid_file(){
    echo `get_pid_file_name`
}

get_pid_file_name(){
    pwd | sed -e 's/[\\.\\/]/_/g' | sed -e 's/$/syncsyncgit.log/'
}

sync(){
    git pull --ff 2>&1 | grep -v "^Already up-to-date.$"
    git add . 2>&1
    local dry_run=`commit --porcelain 2>&1`
    if [ -n "$dry_run" ]
    then
        echo $dry_run
        commit
    fi
    git push --quiet 2>&1 | grep -v "^Everything up-to-date$"
}

help(){
    echo -n "\
$0 {start|stop|sync}
  start: start sync
  stop: stop sync
  sync: do sync just one time
"
}

commit(){
    options="$*"
    git commit --all --message "`date +'%F %T'` $0" $options 2>&1 |\
      grep -v "^# On branch master$" |\
      grep -v "^nothing to commit (working directory clean)$"
 }
 
main "$@"
