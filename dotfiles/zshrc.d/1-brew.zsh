if ! has_command brew && [[ -d /opt/homebrew ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

has_command brew || return

path=(
    $HOME/.{settings,dotfiles}/bin(N-/)
    $HOME/.local/bin(N-/)
    #$(brew --prefix)/bin(N-/)
    $(brew --prefix)/opt/*/libexec/gnubin
    $path
)

manpath=(
    #$(brew --prefix)/share/man(N-/)
    $(brew --prefix)/opt/*/libexec/gnuman
    $manpath
)
