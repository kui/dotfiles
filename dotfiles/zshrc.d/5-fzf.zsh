has_command fzf || return

source_if_exist ~/.fzf.zsh

export FZF_DEFAULT_OPTS="--exact --reverse --color=dark,hl:177,fg+:82,hl+:207"

fzf-ps() {
    local p="$(ps -xww -opid -ocommand | sed 1d | \
               fzf-tmux --prompt='process > ' -n2.. | \
               perl -ne '/^\s*([\d]*)\s/; print $1, " " if $1')"
    if [ -n "$p" ]; then
        LBUFFER="${LBUFFER}${p:0:-1}"
        zle redisplay
    fi
}
zle -N fzf-ps

fzf-search-file() {
    local f="$(find -L . -type f -exec sh -c "file '{}' | grep -q text" \; -print | \
               xargs -I{} bash -c "cat '{}' | sed 's|^|{}: |'" | \
               fzf-tmux --prompt='search file > ' | \
               cut -d: -f1)"
    if [ -n "$f" ]; then
        LBUFFER="${LBUFFER}$f"
        zle redisplay
    fi
}
zle -N fzf-search-file

fzf-find-file() {
    local f="$(fzf-ff)"
    LBUFFER="${LBUFFER}$f"
    zle redisplay
}
zle -N fzf-find-file

fzf-exec() {
    local c="$(zle -la | grep -v '^\.' | fzf-tmux --prompt='exec widget > ')"
    [ -n "$c" ] && zle "$c"
    zle redisplay
}
zle -N fzf-exec

fzf-cd-history() {
    local d="$(cdr -l | sed -r 's/^[0-9]+ +//' | \
               fzf-tmux --prompt='cd hitory > ' --no-sort)"
    if [ -n "$d" ]; then
        LBUFFER="cd $d"
        zle accept-line
    else
        zle redisplay
    fi
}
zle -N fzf-cd-history

fzf-history-widget() {
  local selected num
  selected=(
      $(fc -l 1 | \
               fzf-tmux +s --tac +m -n2..,.. \
                        --tiebreak=index \
                        --toggle-sort=ctrl-r \
                        --prompt='history > ' \
                        -q "${LBUFFER//$/\\$}")
  )
  if [ -n "$selected" ]; then
    num=$selected[1]
    if [ -n "$num" ]; then
      zle vi-fetch-history -n $num
    fi
  fi
  zle redisplay
}
zle -N fzf-history-widget

if has_command brew; then
    source_if_exist "$(brew --prefix)"/opt/fzf/shell/completion.zsh
fi

if [[ -x "$(which fzf-tmux)" ]]; then
    bindkey '^xp' fzf-ps
    bindkey '^xs' fzf-search-file
    bindkey '^x^f' fzf-find-file
    bindkey '^[x' fzf-exec
    bindkey '^[i' fzf-cd-history
    bindkey '^r' fzf-history-widget
fi
