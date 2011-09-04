#!/bin/sh
#
#  you can use a git repository as Dropbox
#

INTERVAL="60"
LOG_DIR="$HOME/local/log/"
PID_DIR="$HOME/local/var/"

GC_INTERVAL=20

main(){

    cd `dirname $0`

    get_pid_file_name
    get_log_file_name
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

    echo -n "start: "
    
    count=0
    log_file=`get_log_file_name`
    check_dir "$log_file"

    while true
    do
        sync | logger "$log_file"
        if [ $count -gt $GC_INTERVAL ]
        then
            git gc 2>&1 | logger "$log_file"
            echo "git gc" | logger "$log_file"
            count=0
        fi
        sleep $INTERVAL
        count=$[$count+1]
    done &

    pid=$1

    if [ $? -eq 0 ]
    then
        echo "OK"
        create_pid_file $!
    else
        echo "FALSE"
        exit 1
    fi
    
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
    sed -e "s/^/$datetime /" >> $1
}

sigint_hook(){
    echo 
    echo "exit $0"
    exit 0
}

check_pid_file(){
    local pid_file=`get_pid_file_name`
    if [ -f "$pid_file" ]
    then
        echo "error: Already started (pid:`get_pid`)" >&2
        exit 1
    fi
}
create_pid_file(){
    local pid_file=`get_pid_file_name`
    check_dir "$pid_file"
    echo "$1"  > "$pid_file"
}

get_pid(){
    local pid_file=`get_pid_file_name`
    cat "$pid_file"
}

delete_pid_file(){
    local pid_file=`get_pid_file_name`
    rm "$pid_file"
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
        local dir="$pid_dir/"
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
        $cmd
    fi
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

commit(){
    options="$*"
    git commit --all --message "`date +'%F %T'` $0" $options 2>&1 |\
      grep -v "^# On branch master$" |\
      grep -v "^nothing to commit (working directory clean)$"
}

help(){
    echo -n "\
$0 {start|stop|sync}
  start: start sync
  stop: stop sync
  sync: do sync just one time
"
}

 
main "$@"
