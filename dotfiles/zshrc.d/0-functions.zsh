source_if_exist() {
    if [[ ! -f "$1" ]]; then
        return 1
    fi

    echo "source: $1"

    local start_at
    start_at=$(now_milli)

    source "$@"

    echo "  Exit Code: $?, Elapsed Time: $(($(now_milli) - $start_at)) ms"
}

now_milli() {
    echo $(( $(now_nano) / 1000 / 1000 ))
}

now_nano() {
    local n
    n=$(date +%s%N)
    if [[ $n =~ ^[0-9]+$ ]]; then
        echo $n
        return
    fi

    if has_command brew; then
        gdate +%s%N
        return
    fi

    if has_command python; then
        python -c 'import time; print(int(time.time() * 1000000000))'
        return
    fi

    if has_command python3; then
        python3 -c 'import time; print(int(time.time() * 1000000000))'
        return
    fi

    date +%s000000000
}

has_command() {
    command -v "$1" >/dev/null 2>&1
}

readlink_f() {
    has_command greadlink && greadlink -f "$1" && return
    has_command readlink && readlink -f "$1" && return
    has_command python && python -c "import os; print(os.path.realpath('$1'))"
    python3 -c "import os; print(os.path.realpath('$1'))"
}

jqess() {
    jq -C | less -R
}

# HTTPサーバを立ち上げる
serve() {
    local port=${SERVE_PORT:-8007}

    local current_dir="$(pwd)"
    local dir file
    if [[ $# -eq 0 ]]; then
        # no-op
    elif [[ -d "$1" ]]; then
        dir="$1"
    elif [[ -f "$1" ]]; then
        dir="$(dirname "$1")"
        file="$(basename "$1")"
    else
        echo "Not Found $1" >&2
        return 1
    fi

    if [[ -n "$dir" ]]
    then
        color_echo green "Serve $dir"
        cd "$dir"
    else
        color_echo green "Serve the current directory"
    fi
    if has_command python; then
        python -m SimpleHTTPServer $port &
    elif has_command python3; then
        python3 -m http.server $port &
    elif has_command ruby; then
        ruby -run -e httpd . -p $port &
    elif has_command busybox; then
        busybox httpd -f -p $port &
    else
        echo "Not Found python, ruby, busybox" >&2
        return 1
    fi
    cd "${current_dir}"

    local url="http://localhost:$port/$file"
    color_echo green "Open $url"
    if which sensible-browser &> /dev/null; then
        sensible-browser "$url"
    elif which xdg-open &> /dev/null; then
        xdg-open "$url"
    fi

    fg
}
alias http-serve=serve

list_parents() {
    local p="${1:=$PWD}"
    printf "$p\n"
    if [[ "$p" != "/" ]]; then
        list_parents "$(dirname "$p")"
    fi
}

## 色一覧
color_list() {
    for c in {000..015}
    do
        print -nP "%F{${c}} $c"
        [[ $((c%8)) -eq 7 ]] && print
    done

    for c in {016..255}
    do
        print -nP "%F{${c}} $c"
        [[ $(((c-16)%6)) -eq 5 ]] && print
    done
}

color_echo() {
    if [[ $# -lt 1 ]]
    then
        (
            echo "usage: $0 COLOR [string...]"
            echo "  COLOR: black, gray, red, green, blue, brown, purple, cyan, [0-7]"
            echo "sample 1: $0 red foo"
            echo "sample 2: $0 1 foo"
        ) >&2
        return 1
    fi

    local c="$1"; shift
    case "$c" in
        black)  c="0";;
        red)    c="1";;
        green)  c="2";;
        brown)  c="3";;
        blue)   c="4";;
        purple) c="5";;
        cyan)   c="6";;
        gray)   c="7";;
        [1-7])  ;; # no-op
        *)
            echo "unknown COLOR: $c"
            return 1
    esac

    echo "\e[3${c}m$@\e[39m"
}

## 256色生成用便利関数
### red: 0-5
### green: 0-5
### blue: 0-5
color256() {
    echo -n $(($1 * 36 + $2 * 6 + $3 + 16))
}

color256_echo() {
    if [[ $# -lt 3 ]]
    then
        (
            echo "usage: $0 RED GREEN BLUE [string...]"
            echo "  RED, GREEN, BLUE: 0-5"
            echo "sample: $0 0 5 5 foo"
        ) >&2
        return 1
    fi

    local r=$1; shift
    local g=$1; shift
    local b=$1; shift

    echo "\e[38;5;$(color256 $r $g $b)m$@\e[39m"
}
