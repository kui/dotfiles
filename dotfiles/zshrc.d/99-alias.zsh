alias less="less -R"
alias grep="grep --color"

if   ls -F --color &>/dev/null;  then alias ls="ls -F --color=auto"
elif gls -F --color &>/dev/null; then alias ls="gls -F --color=auto"
elif ls -F -G &>/dev/null;       then alias ls="ls -F -G"
fi

if has_command reattach-to-user-namespace; then
    alias terminal-notifier='reattach-to-user-namespace terminal-notifier'
fi

# open
if has_command xdg-open; then
    alias open=/usr/bin/xdg-open
elif has_command powershell.exe; then
    alias open="powershell.exe /C start"
fi
alias o=open

if has_command code; then
    alias e="code"
fi
