PROMPT=$'\n'$PromptUserColor'${USER}@${HOST} '$Yellow'%~ '$'\n'$Default'%(!.#.$) '
RPROMPT=
colors=(
    $'%{\e[0;31m%}' # red
    $'%{\e[0;32m%}' # green
    $'%{\e[0;33m%}' # brown
    $'%{\e[0;34m%}' # blue
    $'%{\e[0;35m%}' # purple
    $'%{\e[0;36m%}' # cyan
    
    # light colors
    $'%{\e[1;31m%}' # red
    $'%{\e[1;32m%}' # green
    $'%{\e[1;33m%}' # brown
    $'%{\e[1;34m%}' # blue
    $'%{\e[1;35m%}' # purple
    $'%{\e[1;36m%}' # cyan
)
_get_host_color(){
    local hash=0
    for i in `echo -n $HOST | hexdump -e '"" 10/1 " %03d" '`
    do
        local hash=$(($hash+$i))
        echo $hash
    done
    local hash=$((${hash}%${#colors}))
    echo $colors[$hash]
}
update_prompt(){
    local datetime="`date +'%Y/%m/%d %H:%M:%S'`"
    local escaped_home="`echo ${HOME}|sed -e 's/\//\\\\\//g'`"
    local current_path="`pwd|sed -e \"s/^${escaped_home}/~/\"`"

    local left=$'%{\e[1;36m%}'"${USER}@${HOST}"$'%{\e[1;39m%}'":"$'%{\e[1;33m%}'"${current_path} "
    local right=$'%{\e[1;30m%}'" ${datetime}"
    local num_bar=`print -n -P -- "$left$right" | sed -e $'s/\e\[[0-9;]*m//g' | wc -m | sed -e 's/ //g'`
    local num_bar=$((${COLUMNS}-${num_bar}))

    if [ $num_bar -lt 0 ]
    then
        local datetime="`date +'%H:%M:%S'`"
        local right=$'%{\e[1;30m%}'" ${datetime}"
        local num_bar=`print -n -P -- "$left$right" | sed -e $'s/\e\[[0-9;]*m//g' | wc -m | sed -e 's/ //g'`
        local num_bar=$((${COLUMNS}-${num_bar}))
        if [ $num_bar -lt 0 ]
        then
            local right=''
            local num_bar=`print -n -P -- "$left$right" | sed -e $'s/\e\[[0-9;]*m//g' | wc -m | sed -e 's/ //g'`
            local num_bar=$((${COLUMNS}-${num_bar}))
        fi
    fi

    local sep=" -"
    local bar=""
    while [ $num_bar -gt $((${#bar}+${#sep})) ]
    do
        local bar="$bar$sep"
    done
    while [ $num_bar -gt ${#bar} ]
    do
        local bar="$bar "
    done
    PROMPT=$'\n'"${left}"$'%{\e[1;30m%}'"${bar}${right}"$'\n'"${Default}\$ "
}
# precmd_functions=($precmd_functions update_prompt)
