# -*- mode: sh; coding: utf-8; -*-
pixels-fortune

if [ $TERM = "xterm" ] && infocmp xterm-256color &>/dev/null; then
    export TERM="xterm-256color"
fi

stty stop undef

## 補完機能の強化
autoload -U compinit
compinit -u

############################################################
## alias, funcction の設定

alias less="less -R"
alias grep="grep --color"

if   ls -F --color &>/dev/null;  then alias ls="ls -F --color=auto"
elif gls -F --color &>/dev/null; then alias ls="gls -F --color=auto"
elif ls -F -G &>/dev/null;       then alias ls="ls -F -G"
fi

if which reattach-to-user-namespace &>/dev/null
then
    alias terminal-notifier='reattach-to-user-namespace terminal-notifier'
fi

eval $(dircolors)

if which emacs &> /dev/null && which emacsclientw &> /dev/null; then
    alias e="emacsclientw --no-wait"
    export EDITOR=emacsclientw
elif which code &> /dev/null; then
    alias e="code"
    export EDITOR="code --wait"
fi

if clipboard test &>/dev/null; then
    if [[ -n "$TMUX" ]]; then
        tmux bind-key C-y run 'clipboard paste | tmux load-buffer - && tmux paste-buffer'
        tmux bind-key -Tcopy-mode C-w send-keys -X copy-pipe-and-cancel 'clipboard copy'
        tmux bind-key -Tcopy-mode M-w send-keys -X copy-pipe-and-cancel 'clipboard copy'
    fi
fi

# open
if [[ -f /usr/bin/xdg-open ]]
then alias open=/usr/bin/xdg-open
fi
alias o=open

############################################################
##  cdr の設定
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 1000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both

############################################################
##  zsh プロパティ

HISTFILE=$HOME/.zsh-history
HISTSIZE=100000
SAVEHIST=100000

## コアダンプサイズを制限
limit coredumpsize 102400

## 出力の文字列末尾に改行コードが無い場合でも表示
unsetopt promptcr

## 色を使う
setopt prompt_subst

## ビープを鳴らさない
setopt nobeep

## 内部コマンド jobs の出力をデフォルトで jobs -l にする
setopt long_list_jobs

## 補完候補一覧でファイルの種別をマーク表示
setopt list_types

## サスペンド中のプロセスと同じコマンド名を実行した場合はリジューム
# setopt auto_resume

## 補完候補を一覧表示
setopt auto_list

## 直前と同じコマンドをヒストリに追加しない
setopt hist_ignore_dups

## cd 時に自動で push
setopt autopushd

## 同じディレクトリを pushd しない
setopt pushd_ignore_dups

## ファイル名で #, ~, ^ の 3 文字を正規表現として扱う
setopt extended_glob

## TAB で順に補完候補を切り替える
setopt auto_menu

## zsh の開始, 終了時刻をヒストリファイルに書き込む
setopt extended_history

## =command を command のパス名に展開する
setopt equals

## --prefix=/usr などの = 以降も補完
setopt magic_equal_subst

## ヒストリを呼び出してから実行する間に一旦編集
setopt hist_verify

## ファイル名の展開で辞書順ではなく数値的にソート
setopt numeric_glob_sort

## 出力時8ビットを通す
setopt print_eight_bit

## ヒストリを共有
setopt share_history

## command > file によるファイル上書きを禁止
setopt noclobber

## 補完候補のカーソル選択を有効に
zstyle ':completion:*:default' menu select=1

## 補完候補の色づけ
zstyle ':completion:*:default' list-colors "${LS_COLORS}"

## ディレクトリ名だけで cd
# setopt auto_cd

## カッコの対応などを自動的に補完
setopt auto_param_keys

## ディレクトリ名の補完で末尾の / を自動的に付加し、次の補完に備える
setopt auto_param_slash

## スペルチェック
#setopt correct

## sudo時にはsudo用のパスも使う。
zstyle ':completion:sudo:*' environ PATH="$SUDO_PATH:$PATH"

## 特定のコマンドの補完を無効化
# compdef -d java

# エンターキー入力時の挙動
do_enter() {
    if [[ -n "$BUFFER" ]]
    then
        zle accept-line
        return 0
    fi

    echo

    local threshold=20
    if [[ $(ls -A1 | wc -l) -lt $threshold ]]
    then ls -Ahl
    elif [[ $(ls -1 | wc -l) -lt $threshold ]]
    then ls -hl
    else ls
    fi

    if [[ -n "$VCS_NAME" ]]
    then
        echo
        echo -e "\e[0;33m### $VCS_NAME status #################\e[0m"
        case "$VCS_NAME" in
            git | svn ) which "$VCS_NAME" > /dev/null 2>&1 && "$VCS_NAME" status ;;
        esac
    fi

    if [[ $( jobs | wc -l ) -gt 0 ]]
    then
        echo
        echo -e "\e[0;33m### jobs #############################\e[0m"
        jobs
    fi

    echo -e "\n\n"
    zle reset-prompt
    return 0
}
zle -N do_enter
bindkey '^m' do_enter

# syntax-highligting
source_if_exist "$HOME/.zshrc.d/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh"

# racer: see installs/racer.sh
export RUST_SRC_PATH="$HOME/.local/rust/src"

# direnv
if which direnv &> /dev/null
then
    eval "$(direnv hook zsh)"
fi
