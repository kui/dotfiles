#!/bin/sh
#
#  you can use a git repository as Dropbox
#

# sync interval
INTERVAL=60

LOG_DIR="$HOME/local/var/log"
PID_DIR="$HOME/local/var/run"

GC_INTERVAL=20

main(){

    cd `dirname $0`

    PID_FILE=`get_pid_file_name`
    LOG_FILE=`get_log_file_name`
    case $1 in
        start) run ;;
        stop) stop ;;
        sync) sync ;;
        log) cat_log ;;
        *) help;;
    esac

}

run(){

    if ! is_git_dir
    then
        echo "the current dir is not a git ripository" >&2
        exit 1
    fi

    is_already_started

    count=0
    check_dir "$LOG_FILE"

    echo -n "start: "

    echo "start sync" | logger
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

    pid=$!

    if [ $? -eq 0 ]
    then
        echo "OK"
        create_pid_file $pid
    else
        echo "FALSE"
        exit 1
    fi

}

stop(){
    local pid=`get_pid`
    local retry_count=30

    if ! exist_pid $pid
    then
        echo "error: Not started" >&2
        exit 1
    fi

    echo -n "stop: "
    while [ $retry_count -gt 0 ]
    do
        kill -2 $pid
        sleep 0.03
        if ! exist_pid $pid
        then
            break
        fi
        local retry_count=$(($retry_count-1))
    done

    if [ $retry_count -eq 0 ] && (! kill -9 $pid)
    then
        echo "FALSE"
        exit 1
    fi

    echo "OK"
    create_pid_file $!

    delete_pid_file
}

cat_log(){
    cat "$LOG_FILE"
}

exist_pid(){
    local pid=$1
    [ -n "$pid" ] && [ -n "`ps -p $pid -o comm=`" ]
}

is_git_dir(){
    git status > /dev/null 2>&1
}

logger(){
    local datetime=`date +'%F %T'`
    sed -e "s/^/$datetime /" >> "$LOG_FILE"
}

sigint_hook(){
    echo 
    echo "exit $0"
    exit 0
}

check_pid_file(){
    local pid=`get_pid`
    if exist_pid $pid
    then
        echo "error: Already started (pid:$pid)" >&2
        exit 1
    fi
}

create_pid_file(){
    # local pid_file=`get_pid_file_name`
    check_dir "$PID_FILE"
    echo "$1"  > "$PID_FILE"
}

get_pid(){
    # local pid_file=`get_pid_file_name`
    cat "$PID_FILE" 2> /dev/null
}

delete_pid_file(){
    # local pid_file=`get_pid_file_name`
    rm "$PID_FILE"
}

get_log_file_name(){
    get_file_name $LOG_DIR "log"
}

get_pid_file_name(){
    get_file_name $PID_DIR "pid"
}

get_file_name(){
    local dir=$1
    local suffix=$2
    if ! echo $dir | grep "/$" > /dev/null 2>&1
    then
        local dir="$dir/"
    fi
    echo "$dir`get_base_file_name`.$suffix"
}

get_base_file_name(){
    pwd | sed -e 's/[\\.\\/]/_/g' |\
      sed -e 's/$/_syncsyncgit/'
}

check_dir(){
    local file=$1
    local dir=`dirname "$file"`
    if ! [ -d "$dir" ]
    then
        local cmd="mkdir -p \"$dir\""
        echo $cmd
        eval $cmd
    fi
}

sync(){
    git pull --ff 2>&1 | grep -v "^Already up-to-date.$"
    git add . 2>&1
    local dry_run=`commit --porcelain 2>&1`
    if [ -n "$dry_run" ]
    then
        echo $dry_run
        commit --quiet
    fi
    git push --quiet 2>&1 | grep -v "^Everything up-to-date$"
}

commit(){
    local options="$@"
    git commit --all --message "`date +'%F %T'` $0" $options 2>&1 |\
      grep -v "^# On branch master$" |\
      grep -v "^nothing to commit (working directory clean)$"
}

help(){
    echo -n "\
$0 {start|stop|sync|log}
  start: start sync
  stop: stop sync
  sync: do sync just one time
  log: show log
"
}

 
main "$@"
