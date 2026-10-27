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

# Use only fixture SSH configs, not the operator's workspace inventory.
set -g __wmux_test_home (mktemp -d)
function __wmux_test_cleanup --on-event fish_exit
    command rm -rf -- "$__wmux_test_home"
end
mkdir -p "$__wmux_test_home/.ssh/workspaces"
printf '%s\n' 'Host workspace-test-one' >"$__wmux_test_home/.ssh/workspaces/test-one.config"
printf '%s\n' 'hOsT = "workspace-test-two" another-alias # comment' >"$__wmux_test_home/.ssh/workspaces/test-two.config"
printf '%s\n' 'Host workspace-*' >"$__wmux_test_home/.ssh/workspaces/wildcard.config"
printf '%s\n' 'Match all' >"$__wmux_test_home/.ssh/workspaces/00-common.config"
printf '%s\n' 'Host workspace-other-name' >"$__wmux_test_home/.ssh/workspaces/mismatch.config"
printf '%s\n' 'Host "unterminated' >"$__wmux_test_home/.ssh/workspaces/broken.config"

function ssh
    echo 'FAIL: completion executed ssh' >&2
    exit 1
end

begin
    set -lx HOME "$__wmux_test_home"
    assert_completion 'wmux attach ' test-one:
    assert_completion 'wmux attach test-t' test-two:
    assert_completion 'wmux attach --workspace ' test-one
    assert_completion 'wmux attach --workspace=test-t' --workspace=test-two
    assert_no_completion 'wmux attach ' wildcard:
    assert_no_completion 'wmux attach ' 00-common:
    assert_no_completion 'wmux attach ' mismatch:
    assert_no_completion 'wmux attach ' broken:
    assert_no_completion 'wmux attach --workspace ' test-one:
    pushd "$root"
    assert_no_completion 'wmux attach --workspace ' wmux.fish
    popd
    assert_no_completion 'wmux attach target ' test-one:
    assert_no_completion 'wmux attach -- ' test-one:
    assert_no_completion 'wmux list ' test-one:
    rm -rf -- "$__wmux_test_home/.ssh/workspaces"
    assert_no_completion 'wmux attach ' test-one:
end

echo 'PASS: wmux completions (34 checks; no wmux or SSH calls)'
