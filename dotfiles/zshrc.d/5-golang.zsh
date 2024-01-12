export GOPATH="$HOME/golang"

path=(
    $GOPATH/bin(N-/)
    /usr/local/go/bin(N-/)
    $path
)

# golang utillityes which be installed by dpkg/apt
source_if_exist "/usr/lib/go/misc/zsh/go"
