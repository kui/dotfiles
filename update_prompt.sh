PROMPT=$'\n'$PromptUserColor'${USER}@${HOST} '$Yellow'%~ '$'\n'$Default'%(!.#.$) '
RPROMPT=

update_prompt(){
    local datetime="`date +'%Y/%m/%d %H:%M:%S'`"
    local escaped_home="`echo ${HOME}|sed -e 's/\//\\\\\//g'`"
    local current_path="`pwd|sed -e \"s/^${escaped_home}/~/\"`"
    local num_bar=
    PROMPT="$current_path "$'\n'"$Default\$ "
}