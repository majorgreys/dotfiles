function dl --description "list dtach sessions"
    set -l socket_dir /tmp/dtach
    if not test -d $socket_dir; or test -z "$(ls -A $socket_dir 2>/dev/null)"
        echo "No sessions"
        return
    end
    for socket in $socket_dir/*
        echo (basename $socket)
    end
end
