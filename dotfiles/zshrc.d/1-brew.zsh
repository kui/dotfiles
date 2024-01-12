has_command brew || return

path=(
    $(brew --prefix)/bin(N-/)
    $(brew --prefix)/opt/*/libexec/gnubin
    $path
)

manpath=(
    $(brew --prefix)/share/man(N-/)
    $(brew --prefix)/opt/*/libexec/gnuman
    $manpath
)
