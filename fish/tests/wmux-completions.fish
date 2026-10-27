# Run with: fish --no-config fish/tests/wmux-completions.fish
set -l root (path dirname (status filename))/../.config/fish/completions
source $root/wmux.fish

# Completing commands must never execute the program.
function wmux
    echo 'FAIL: completion executed wmux' >&2
    exit 1
end

function assert_completion --argument-names line expected
    set -l candidates (complete -C "$line" | string split -f 1 \t)
    if not contains -- "$expected" $candidates
        printf 'FAIL: %s did not offer %s\n' "$line" "$expected" >&2
        exit 1
    end
end

function assert_no_completion --argument-names line unexpected
    set -l candidates (complete -C "$line" | string split -f 1 \t)
    if contains -- "$unexpected" $candidates
        printf 'FAIL: %s offered %s\n' "$line" "$unexpected" >&2
        exit 1
    end
end

assert_completion 'wmux ' attach
assert_completion 'wmux att' attach
assert_completion 'wmux attach --w' --workspace
assert_completion 'wmux list --' --json
assert_completion 'wmux auth ' status
assert_completion 'wmux auth secret ' set
assert_completion 'wmux configure issue-agent ' status
assert_completion 'wmux auth --workspace example secret ' set
assert_completion 'wmux auth --workspace=example secret ' set
assert_completion 'wmux review --harness ' claude
assert_completion 'wmux auth github-workspace-login --account ' managed
assert_completion 'wmux configure delegation-focus ' never
assert_completion 'wmux help ' attach
assert_completion 'wmux docs ' create
assert_completion "wmux dotfiles add --path $root/wm" "$root/wmux.fish"
assert_no_completion 'wmux auth --workspace ' status
assert_no_completion 'wmux attach --' --repo
assert_no_completion 'wmux ' __agent
assert_no_completion 'wmux delegate -- ' status
assert_no_completion 'wmux delegate -- --' --workspace

echo 'PASS: wmux completions (20 checks; no wmux calls)'
