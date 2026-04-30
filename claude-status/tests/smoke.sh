#!/usr/bin/env bash
# Smoke tests for claude-status helpers.
#
# Run with: ./tests/smoke.sh
#
# Each test_* function is one scenario. Add new scenarios as separate
# functions and append the function name to the TESTS array at the
# bottom.

set -euo pipefail

TESTS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=test-helpers.sh
source "$TESTS_DIR/test-helpers.sh"

# (test_* functions appended in later tasks)

test_helper_writes_json_with_cwd() {
  echo '{"session_id":"abc12345","cwd":"/Users/me/proj-one"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" running

  local f="$XDG_STATE_HOME/sketchybar/sessions/abc12345.json"
  assert_file "$f"

  assert_eq "$(jq -r .session_id "$f")" "abc12345"  "session_id"
  assert_eq "$(jq -r .workspace "$f")"  "3"         "workspace"
  assert_eq "$(jq -r .state "$f")"      "running"   "state"
  assert_eq "$(jq -r .cwd "$f")"        "/Users/me/proj-one" "cwd"

  # updated_at must be a recent epoch second
  local ts
  ts="$(jq -r .updated_at "$f")"
  [ "$ts" -gt 0 ] || fail "updated_at not numeric: $ts"

  # The per-workspace agent file is unchanged behavior — still written.
  assert_file "$XDG_STATE_HOME/sketchybar/agents/3"
  assert_eq "$(< "$XDG_STATE_HOME/sketchybar/agents/3")" "running" "agent file"
}

TESTS=(
  test_helper_writes_json_with_cwd
)

main() {
  local failed=0
  for t in "${TESTS[@]}"; do
    echo "==> $t"
    (
      setup
      trap teardown EXIT
      "$t"
    ) || failed=$((failed + 1))
  done
  if [ "$failed" -gt 0 ]; then
    echo "$failed test(s) failed" >&2
    exit 1
  fi
  echo "all tests passed"
}

main "$@"
