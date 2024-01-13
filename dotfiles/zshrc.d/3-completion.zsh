autoload -U compinit

# 初期化
# https://zsh.sourceforge.io/Doc/Release/Completion-System.html
# > To avoid these tests and make all files found be used without asking, use the option -u,
compinit -u

# 補完候補の詳細表示
zstyle ':completion:*' verbose yes
#zstyle ':completion:*' verbose no

# 補完機能
# completer の各機能についてはドキュメントの "20.4 Control Functions" を参照
# https://zsh.sourceforge.io/Doc/Release/Completion-System.html#:~:text=%5B%20%3F%20%5D-,20.4%20Control%20Functions,-The%20initialization%20script
zstyle ':completion:*' completer _complete _ignored _match _oldlist

# 補完候補のカーソル選択を有効に
zstyle ':completion:*:default' menu select=1

# 補完候補の色づけ
zstyle ':completion:*:default' list-colors "${LS_COLORS}"

# sudo時にはsudo用のパスも使う。
zstyle ':completion:sudo:*' environ PATH="$SUDO_PATH:$PATH"

# description フォーマット
zstyle ':completion:*:descriptions' format '[%d]'

# キャッシュ
zstyle ':completion:*' use-cache on
