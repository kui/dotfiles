PROMPT=$'\n'$PromptUserColor'${USER}@${HOST} '$Yellow'%~ '$'\n'$Default'%(!.#.$) '
RPROMPT=

update_prompt(){
    local datetime="`date +'%Y/%m/%d %H:%M:%S'`"
    local escaped_home="`echo ${HOME}|sed -e 's/\//\\\\\//g'`"
    local current_path="`pwd|sed -e \"s/^${escaped_home}/~/\"`"

    local left="`fg256 3 2 0`${USER}@${HOST}${Default}:`fg256 5 0 0`${current_path}"
    local right="`fg256 3 3 3`${datetime}"
    local num_bar=$(())
    PROMPT="$left "$'\n'"$Default\$ "
}