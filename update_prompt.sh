PROMPT=$'\n'$PromptUserColor'${USER}@${HOST} '$Yellow'%~ '$'\n'$Default'%(!.#.$) '
RPROMPT=$DarkGray'%D [ %T ] '$Default

update_prompt(){
    local datetime=`date +'%Y/%m/%d %H:%M:%S'`
}