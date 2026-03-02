function dac --description "dtach session for claude code"
    set -l socket_dir /tmp/dtach
    mkdir -p $socket_dir

    set -l session_name claude-(basename (pwd))
    if set -q argv[1]
        set session_name claude-$argv[1]
    end

    set -l socket $socket_dir/$session_name
    dtach -A $socket -z claude $argv[2..]
end
