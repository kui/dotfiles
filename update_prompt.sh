PROMPT=$'\n'$PromptUserColor'${USER}@${HOST} '$Yellow'%~ '$'\n'$Default'%(!.#.$) '
RPROMPT=

update_prompt(){
    local datetime="`date +'%Y/%m/%d %H:%M:%S'`"
    local escaped_home="`echo ${HOME}|sed -e 's/\//\\\\\//g'`"
    local current_path="`pwd|sed -e \"s/^${escaped_home}/~/\"`"

    local left=$'%{\e[1;36m%}'"${USER}@${HOST}${Default}:"$'%{\e[1;33m%}'"${current_path}"
    local right="`fg256 0 0 1`${datetime}"
    local num_bar=$(())
    PROMPT="$left "$'\n'"$Default\$ "
}