# tmux の設定は基本的に tmux.conf に書く
# ここは実行環境から動的に tmux の設定をするための場所

if clipboard test &>/dev/null; then
    tmux bind-key C-y run 'clipboard paste | tmux load-buffer - && tmux paste-buffer'
    tmux bind-key -Tcopy-mode C-w send-keys -X copy-pipe-and-cancel 'clipboard copy'
    tmux bind-key -Tcopy-mode M-w send-keys -X copy-pipe-and-cancel 'clipboard copy'
fi
