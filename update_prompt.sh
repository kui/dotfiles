PROMPT=$'\n'$PromptUserColor'${USER}@${HOST} '$Yellow'%~ '$'\n'$Default'%(!.#.$) '
RPROMPT=

update_prompt(){
    local datetime="`date +'%Y/%m/%d %H:%M:%S'`"
    local escaped_home="`echo ${HOME}|sed -e 's/\//\\\\\//g'`"
    local current_path="`pwd|sed -e \"s/^${escaped_home}/~/\"`"

    local left=$'%{\e[1;36m%}'"${USER}@${HOST}${Default}:"$'%{\e[1;33m%}'"${current_path} "
    local right=$'%{\e[0;37m%}'" ${datetime}"
    local num_bar=$((${COLUMNS}-${#left}-${#right}))
    local bar=""

    while [ num_bar -gt 0 ]
    do
        local bar="$bar="
        local num_bar=$((${num_bar}-1))
    done
    PROMPT="$left "$'\n'"$Default\$ "
}