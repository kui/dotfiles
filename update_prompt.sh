PROMPT=$'\n'$PromptUserColor'${USER}@${HOST} '$Yellow'%~ '$'\n'$Default'%(!.#.$) '
RPROMPT=

update_prompt(){
    local datetime="`date +'%Y/%m/%d %H:%M:%S'`"
    local escaped_home="`echo ${HOME}|sed -e 's/\//\\\\\//g'`"
    local current_path="`pwd|sed -e \"s/^${escaped_home}/~/\"`"

    local left=$'%{\e[1;36m%}'"${USER}@${HOST}${Default}:"$'%{\e[1;33m%}'"${current_path}  "
    local right=$'%{\e[0;37m%}'"  ${datetime}"
    local num_bar=`print -n -P -- "$left$right" | sed -e $'s/\e\[[0-9;]*m//g' | wc -m | sed -e 's/ //g'`
    local num_bar=$((${COLUMNS}-${num_bar}))

    local sep=" * "
    local bar=""
    while [ $num_bar -gt $((${#bar}+${#sep})) ]
    do
        local bar="$bar$sep"
    done

    PROMPT=$'\n'"${left}"$'%{\e[1;30m%}'"${bar}${right}"$'\n'"${Default}\$ "
}