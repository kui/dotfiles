#############################################################
# 時間のかかったコマンドは、通知システムに通知する

TIMETRACK_THRESHOLD=10 # seconds
export TIMETRACK_THRESHOLD

unset _timetrack_start_time
unset _timetrack_command

_timetrack_start() {
    export _timetrack_command="$1"
    export _timetrack_start_time=$(now_milli)
}

_timetrack_end() {
    unset COMMAND_SECONDS

    if [[ -z "$_timetrack_start_time" ]]; then
        return
    fi

    if [[ -z "$_timetrack_command" ]]; then
        _timetrack_command="<UNKNOWN>"
    fi

    export COMMAND_SECONDS=$(bc <<<"scale=1; $(($(now_milli) - _timetrack_start_time)) / 1000")

    local title="Finished: $_timetrack_command"
    local message="Elapsed Time: ${COMMAND_SECONDS}s"
    if [[ "$COMMAND_SECONDS" -ge "$TIMETRACK_THRESHOLD" ]]; then
        if has_command notify-send && [[ -z "$DISPLAY" ]]; then
            notify-send --icon=terminal "$title" "$message"
        elif has_command terminal-notifier; then
            terminal-notifier -title "$title" -message "$message" -activate "$__CFBundleIdentifier"
        fi
        echo $'\a'
    fi

    unset _timetrack_start_time
    unset _timetrack_command
}

add-zsh-hook preexec _timetrack_start
add-zsh-hook precmd _timetrack_end
