function fish_title
    set -l cwd (prompt_pwd --dir-length 2)
    if set -q argv[1]
        echo "$cwd: $argv[1]"
    else
        echo $cwd
    end
end
