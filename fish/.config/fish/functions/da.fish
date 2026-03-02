function da --description "dtach session tied to current directory"
    set -l socket_dir /tmp/dtach
    mkdir -p $socket_dir

    set -l session_name (string replace -a / - (pwd | string sub -s 2))
    if set -q argv[1]
        set session_name $argv[1]
    end

    set -l socket $socket_dir/$session_name
    dtach -A $socket -z fish
end
