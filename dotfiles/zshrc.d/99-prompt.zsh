# バージョンシステム管理下のディレクトリにいる時の諸々の設定
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git cvs svn
zstyle ':vcs_info:*' get-revision true
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' max-exports 1
zstyle ':vcs_info:*' formats '%f:%F{red}%B%c%u%%b%F{green}%s/%b(%i) '
zstyle ':vcs_info:*' actionformats '%f:%F{red}%B%c%u%b%F{red}%s/%b(%i)/%a '
zstyle ':vcs_info:git:*' formats '%f:%F{red}%B%c%u%%b%F{green}%s/%b '
zstyle ':vcs_info:git:*' actionformats '%f:%F{blue}%B%c%u%%b%F{red}%s/%b/%a '

# 作業コピーに変更があった場合に表示される文字列
zstyle ':vcs_info:*' unstagedstr "*"
# インデックスに追加された場合に表示される文字列
zstyle ':vcs_info:*' stagedstr "+"
# bzr のとき、vcs_info でネットワーク使わせない
zstyle ':vcs_info:bzr:*' use-simple true

ansi_brightness() {
    if [[ 16 -le $1 && $1 -le 231 ]]; then
        local base=$(($1 - 16))
        local r=$((base / 36)) \
              g=$((base % 36 / 6)) \
              b=$((base % 6))
        printf '%f\n' $((r/5.0*299 + g/5.0*587 + b/5.0*114))
        return 0
    elif [[ 232 -le $1 && $1 -le 255 ]]; then
        printf '%f\n' $((255 * ($1 - 232) / (255 - 232)))
        return 0
    else
        echo "Invalid ANSI Color Code: $1" 2>/dev/null
        return 1
    fi
}

# 16 色環境向け
color_codes=(1 2 4 5 6 9 10 12 13 14)

# 256 色環境向け
if [ "$(echotc Co)" = "256" ]; then
    color_codes=()
    for c in {16..255}; do
        b=$(ansi_brightness $c)
        # if [[ $b -le 500 ]] # デフォルトバックグラウンドが明るい色の時
        if [[ $b -ge 500 ]] # デフォルトバックグラウンドが暗い色の時
        then color_codes=($color_codes $c)
        fi
    done
fi
export color_codes

color_code_from_str() {
    local h=$(sum <<< "$1" | cut -f1 -d' ')
    local i=$((h % ${#color_codes}))
    printf ${color_codes[i]}
}

# プロンプトに表示する補足情報
typeset -agx _prompt_opts
_run_all_prompt_opts() {
    for f in "${_prompt_opts[@]}"; "$f"
}

# バックグラウンドジョブ数表示
echo_job_info() {
    print -n '%(1j.%f:%F{red}jobs/%j .)'
}
_prompt_opts+=echo_job_info

# バージョン管理システム情報表示
echo_vcs_info() {
    vcs_info
    print -n "$vcs_info_msg_0_"
}
_prompt_opts+=echo_vcs_info

# rbenv のステータス表示
echo_rbenv_version() {
    has_command rbenv || return
    local v=$(rbenv version-name)
    if [[ "$v" != 'system' ]]; then
        print -n "%f:%F{196}rbenv/$v "
    fi
}
_prompt_opts+=echo_rbenv_version

# pyenv のステータス表示
echo_pyenv_version() {
    has_command pyenv || return
    local v=$(pyenv version-name)
    if [[ "$v" != 'system' ]]; then
        print -n "%f:%F{166}pyenv/$v "
    fi
}
_prompt_opts+=echo_pyenv_version

# kubectl の context 表示
echo_kubectl_context() {
    has_command kubectl || return
    local c="$(kubectl config current-context 2>/dev/null)"
    if [[ -n "$c" ]]; then
        print -n "%f:%F{blue}k8s/$c "
    fi
}
_prompt_opts+=echo_kubectl_context

# 直前のコマンドの実行結果更新
typeset -gx _last_command_status
_update_last_command_status() {
    _last_command_status="time=${LAST_COMMAND_ELAPSED_SECONDS}s exit=$?"
}
add-zsh-hook precmd _update_last_command_status

# プロンプトの構築
user="%F{$(color_code_from_str "$USER")}%n"
host="%F{$(color_code_from_str "$HOST")}%m"
tmux=
if [[ -n "$TMUX" ]]; then
    tmux_str="$(tmux display-message -p '#I.#P')"
    tmux="%f[%F{$(color_code_from_str "${tmux_str}")}${tmux_str}%f]"
fi
PROMPT='%(?.%F{243}.%F{red})%U${(l:COLUMNS:: :)_last_command_status}%u
'"$user"'%f@'"$host$tmux"'%f:%F{yellow}%~ $(_run_all_prompt_opts)
%(?.%f.%F{red})%(!.#.$)%f '
