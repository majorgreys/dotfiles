# Run with: fish --no-config fish/tests/wmux-completions.fish
set -l root (path dirname (status filename))/../.config/fish/completions
source $root/wmux.fish
set -g __wmux_test_checks 0

# Completing commands must never execute the program.
function wmux
    echo 'FAIL: completion executed wmux' >&2
    exit 1
end

function assert_completion --argument-names line expected
    set -g __wmux_test_checks (math $__wmux_test_checks + 1)
    set -l candidates (complete -C "$line" | string split -f 1 \t)
    if not contains -- "$expected" $candidates
        printf 'FAIL: %s did not offer %s\n' "$line" "$expected" >&2
        exit 1
    end
end

function assert_no_completion --argument-names line unexpected
    set -g __wmux_test_checks (math $__wmux_test_checks + 1)
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
    set -lx XDG_CACHE_HOME "$HOME/.cache"
    set -lx WMUX_LOCAL_ROOT ''
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
    # Cached targets supply repos and branches, including branchless checkouts.
    mkdir -p "$XDG_CACHE_HOME/wmux"
    set -l cache "$XDG_CACHE_HOME/wmux/__all__.json"
    printf '%s\n' '{"workspaces": {
        "test-one": {"records": [
            {"workspace_name":"test-one","repo_name":"dogpark","branch":"feature/one"},
            {"repo_name":"dogpark","branch":"main"},
            {"repo_name":"dogpark","branch":"main"},
            {"repo_name":"docs","branch":""},
            {"repo_name":"bad","branch":"bad\nbranch"},
            {"repo_name":"bad","branch":null},
            {"workspace_name":"test-two","repo_name":"wrong","branch":"main"},
            null
        ]},
        "test-two": {"records": [{"repo_name":"wmux","branch":"fix/two"}]},
        "broken": {"records": false}
    }}' >"$cache"
    assert_completion 'wmux attach ' test-one:dogpark:feature/one
    assert_completion 'wmux attach test-one:' test-one:dogpark:
    assert_completion 'wmux attach test-one:dogpark:' test-one:dogpark:main
    assert_completion 'wmux attach test-one:dogpark:f' test-one:dogpark:feature/one
    assert_completion 'wmux attach test-one:docs' test-one:docs:
    assert_completion 'wmux attach dog' dogpark:
    assert_completion 'wmux attach dogpark:f' dogpark:feature/one
    assert_completion 'wmux attach docs:' docs:
    assert_completion 'wmux attach --workspace test-one ' dogpark:main
    assert_completion 'wmux attach --workspace=test-one dogpark:f' dogpark:feature/one
    assert_completion 'wmux attach --workspace test-one test-one:dogpark:' test-one:dogpark:main
    assert_completion 'wmux attach --rescue test-two:' test-two:wmux:fix/two
    assert_no_completion 'wmux attach test-one:' test-two:wmux:fix/two
    assert_no_completion 'wmux attach --workspace test-one ' wmux:fix/two
    assert_no_completion 'wmux attach ' test-one:bad:
    assert_no_completion 'wmux attach ' test-one:wrong:main
    assert_no_completion 'wmux attach --workspace ' test-one:dogpark:main
    assert_no_completion 'wmux attach target ' test-one:dogpark:main
    assert_no_completion 'wmux attach -- ' test-one:dogpark:main

    # Respect alternate cache roots, and do not fall back to another inventory.
    mkdir -p "$HOME/alternate/wmux"
    cp "$cache" "$HOME/alternate/wmux/__all__.json"
    set -lx XDG_CACHE_HOME "$HOME/alternate"
    assert_completion 'wmux attach test-two:' test-two:wmux:fix/two
    set -lx WMUX_LOCAL_ROOT "$HOME/isolated"
    assert_no_completion 'wmux attach test-two:' test-two:wmux:fix/two
    mkdir -p "$WMUX_LOCAL_ROOT/xdg/cache/wmux"
    cp "$cache" "$WMUX_LOCAL_ROOT/xdg/cache/wmux/__all__.json"
    assert_completion 'wmux attach test-two:' test-two:wmux:fix/two
    printf '%s\n' '{broken' >"$WMUX_LOCAL_ROOT/xdg/cache/wmux/__all__.json"
    assert_no_completion 'wmux attach test-two:' test-two:wmux:fix/two
    printf '%s\n' '{"workspaces": []}' >"$WMUX_LOCAL_ROOT/xdg/cache/wmux/__all__.json"
    assert_no_completion 'wmux attach test-two:' test-two:wmux:fix/two
    rm "$WMUX_LOCAL_ROOT/xdg/cache/wmux/__all__.json"
    assert_no_completion 'wmux attach test-two:' test-two:wmux:fix/two
    rm -rf -- "$__wmux_test_home/.ssh/workspaces"
    assert_no_completion 'wmux attach ' test-one:
end

printf 'PASS: wmux completions (%s checks; no wmux or SSH calls)\n' $__wmux_test_checks
