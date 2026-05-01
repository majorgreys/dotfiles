#!/usr/bin/env bash
# Smoke tests for agent-status helpers.
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

# Collect all sketchybar invocations from a single helper run, ignoring
# unrelated commands. Returns the most recent --trigger line.
last_trigger() {
  grep -E '^--trigger agent_state_change' "$SKETCHYBAR_LOG" | tail -n 1
}

test_helper_writes_pin_file_and_triggers_event() {
  echo '{"session_id":"abc12345","cwd":"/Users/me/proj-one"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" running

  # Per-session pin file (one line: workspace number).
  local pin="$XDG_STATE_HOME/sketchybar/sessions/abc12345.ws"
  assert_file "$pin"
  assert_eq "$(< "$pin")" "3" "pin workspace"

  # Triggered event with all kv args.
  local line
  line="$(last_trigger)"
  case "$line" in
    *action=write*session_id=abc12345*state=running*workspace=3*cwd=/Users/me/proj-one*) ;;
    *) fail "expected --trigger with action=write … cwd=…/proj-one but got: $line" ;;
  esac
}

test_helper_purges_legacy_json_records() {
  mkdir -p "$XDG_STATE_HOME/sketchybar/sessions"
  printf '{"junk":1}' > "$XDG_STATE_HOME/sketchybar/sessions/old-id.json"
  printf 'plain'      > "$XDG_STATE_HOME/sketchybar/sessions/older-id"

  echo '{"session_id":"new-id","cwd":"/x"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" running

  assert_no_file "$XDG_STATE_HOME/sketchybar/sessions/old-id.json"
  assert_no_file "$XDG_STATE_HOME/sketchybar/sessions/older-id"
  assert_file    "$XDG_STATE_HOME/sketchybar/sessions/new-id.ws"
}

test_helper_clear_removes_pin_and_triggers_clear() {
  echo '{"session_id":"sid","cwd":"/x"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" running
  assert_file "$XDG_STATE_HOME/sketchybar/sessions/sid.ws"

  echo '{"session_id":"sid"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" clear

  assert_no_file "$XDG_STATE_HOME/sketchybar/sessions/sid.ws"

  local line
  line="$(last_trigger)"
  case "$line" in
    *action=clear*session_id=sid*workspace=3*) ;;
    *) fail "expected --trigger action=clear session_id=sid workspace=3, got: $line" ;;
  esac
}

test_helper_reuses_pinned_workspace_across_calls() {
  # Override the focused workspace mock for a follow-up call. The first
  # call should pin to workspace 3 (mock default); the second call,
  # after we change the mock to 9, should still resolve to 3 from the
  # pin file.
  echo '{"session_id":"pinme","cwd":"/x"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" running

  # Rewrite the aerospace mock to return 9.
  cat > "$MOCK_BIN/aerospace" <<EOF
#!/usr/bin/env bash
if [ "\$1" = "list-workspaces" ] && [ "\$2" = "--focused" ]; then
  echo "9"
fi
exit 0
EOF
  chmod +x "$MOCK_BIN/aerospace"

  echo '{"session_id":"pinme"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" needs-attention

  # The pin was 3; needs-attention call should still target workspace 3.
  local line
  line="$(last_trigger)"
  case "$line" in
    *action=write*session_id=pinme*state=needs-attention*workspace=3*) ;;
    *) fail "expected pinned workspace 3 reused, got: $line" ;;
  esac
}

test_helper_writes_helper_log() {
  echo '{"session_id":"abcd1234efgh","cwd":"/Users/me/p"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" running

  local log="$XDG_STATE_HOME/sketchybar/helper.log"
  assert_file "$log"

  local line
  line="$(tail -n 1 "$log")"
  case "$line" in
    *$'\t'running$'\t'write$'\t'abcd1234$'\t'3$'\t'/Users/me/p) ;;
    *) fail "log line did not match expected fields: $line" ;;
  esac

  echo '{"session_id":"abcd1234efgh"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" clear

  line="$(tail -n 1 "$log")"
  case "$line" in
    *$'\t'clear$'\t'clear$'\t'abcd1234$'\t'3$'\t'*) ;;
    *) fail "clear log line did not match expected fields: $line" ;;
  esac
}

test_helper_skips_when_no_workspace_resolves() {
  # Make aerospace return nothing.
  cat > "$MOCK_BIN/aerospace" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF
  chmod +x "$MOCK_BIN/aerospace"

  echo '{"session_id":"x","cwd":"/x"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" running

  # No pin file written, no trigger fired, log line says skip-no-ws.
  assert_no_file "$XDG_STATE_HOME/sketchybar/sessions/x.ws"
  if [ -s "$SKETCHYBAR_LOG" ]; then
    if grep -Eq -e '^--trigger' "$SKETCHYBAR_LOG"; then
      fail "no --trigger should fire when workspace can't be resolved"
    fi
  fi
  local log_line
  log_line="$(tail -n 1 "$XDG_STATE_HOME/sketchybar/helper.log")"
  case "$log_line" in
    *$'\t'skip-no-ws$'\t'*) ;;
    *) fail "expected skip-no-ws log line, got: $log_line" ;;
  esac
}

TESTS=(
  test_helper_writes_pin_file_and_triggers_event
  test_helper_purges_legacy_json_records
  test_helper_clear_removes_pin_and_triggers_clear
  test_helper_reuses_pinned_workspace_across_calls
  test_helper_writes_helper_log
  test_helper_skips_when_no_workspace_resolves
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
