#!/bin/sh
#
# syncsyncbox.sh
#  use a git repository
#

INTERVAL="60"

GC_INTERVAL=20

main(){

    cd `dirname $0`

    case $1 in
        start) run ;;
        stop) stop ;;
        sync) sync ;;
        *) help;;
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
            echo "git gc" | logger
            count=0
        fi
        sleep $INTERVAL
        count=$[$count+1]
    done &

    create_pid_file $!
    
}

stop(){
    local pid=`get_pid`
    local retry_count=30
    while [ $retry_count -gt 0 ]
    do
        kill -2 $pid
        sleep 0.03
        local retry_count=$(($retry_count-1))
    done
    kill -9 $pid
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
    pid_file=`get_pid_file_name`
    echo "$1"  > "$pid_file"
}

get_pid(){
    pid_file=`get_pid_file_name`
    cat "$pid_file"
}

delete_pid_file(){
    pid_file=`get_pid_file_name`
    rm "$pid_file"
}

get_pid_file_name(){
    pwd | sed -e 's/[\\.\\/]/_/g' |\
      sed -e 's/$/_syncsyncgit.pid/'\
      sed -e 's/^/$HOME\/./'
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
