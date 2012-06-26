#!/bin/sh -eu
# -*- coding:utf-8-unix; mode:sh; -*-
# usage: screen_ssh.sh pid hostname username
#
# .ssh/config の最初の行に下記の三行を追加
#	Host *
#	PermitLocalCommand yes
#	LocalCommand /path/to/screen_ssh.sh $PPID %n %r
#

# 0以外: デバッグ出力する
# 0    : 出力しない
DEBUG=1

d() {
    [ $DEBUG != 0 ] && (echo "$@" | tee $HOME/.ssh/screen_ssh.sh)
}
a() {
    d "abort: $@" >&2
    exit 1
}

# 端末上で実行されてる？
! tty -s && a "stdin is not a terminal"
# [ ! -t 0 ] && a "stdin is not a terminal"

# screen 上で実行されてる？
[ -z "$STY" ] && a "not in screen"

# 引数の数は正しい？
[ $# != "3" ] && a "invalid args"

# SSH は対話モードで実行されてる？（バッチモードで実行されてない？）
grep -a -i "Batchmode yes" /proc/$1/cmdline >/dev/null 2>&1 && a "SSH is in Batch mode"

HOST="$2"
USER="$3"
d "host:	$HOST"
d "user:	$USER"

# タイトルの名前どうする？
TITLE="$USER@$HOST"
d "title:	$TITLE"

# ウィンドウタイトル変更
echo "\033k$TITLE\033\\"
