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

test_helper_purges_legacy_text_pins() {
  mkdir -p "$XDG_STATE_HOME/sketchybar/sessions"
  printf '%s' "5" > "$XDG_STATE_HOME/sketchybar/sessions/legacy-id"
  printf '%s' "7" > "$XDG_STATE_HOME/sketchybar/sessions/another-old"

  echo '{"session_id":"new-id","cwd":"/Users/me/proj"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" running

  assert_no_file "$XDG_STATE_HOME/sketchybar/sessions/legacy-id"
  assert_no_file "$XDG_STATE_HOME/sketchybar/sessions/another-old"
  # New JSON should still be written.
  assert_file "$XDG_STATE_HOME/sketchybar/sessions/new-id.json"
}

test_helper_clear_removes_session_json() {
  echo '{"session_id":"sid","cwd":"/x"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" running
  assert_file "$XDG_STATE_HOME/sketchybar/sessions/sid.json"
  assert_file "$XDG_STATE_HOME/sketchybar/agents/3"

  echo '{"session_id":"sid"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" clear

  assert_no_file "$XDG_STATE_HOME/sketchybar/sessions/sid.json"
  assert_no_file "$XDG_STATE_HOME/sketchybar/agents/3"
}

test_helper_writes_helper_log() {
  echo '{"session_id":"abcd1234efgh","cwd":"/Users/me/p"}' \
    | "$PLUGIN_BIN/sketchybar-set-state" running

  local log="$XDG_STATE_HOME/sketchybar/helper.log"
  assert_file "$log"

  # Last line is the invocation we just made: state=running, action=write,
  # sid8=abcd1234, ws=3 (mock default), cwd=/Users/me/p.
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

test_renderer_zero_sessions_dim_zero() {
  "$PLUGIN_BIN/claude-render-sessions"

  # Parent label set to "0", color = dim grey 0xff7f849c
  assert_sketchybar_logged \
    '--set claude_sessions .*label=0' "label=0"
  assert_sketchybar_logged \
    '--set claude_sessions .*label\.color=0xff7f849c' "dim grey"
}

test_renderer_aggregate_state_yellow() {
  # any needs-attention → parent label color is yellow
  mkdir -p "$XDG_STATE_HOME/sketchybar/sessions"
  jq -n '{session_id:"a",workspace:"3",state:"running",cwd:"/x",updated_at:1}' \
    > "$XDG_STATE_HOME/sketchybar/sessions/a.json"
  jq -n '{session_id:"b",workspace:"5",state:"needs-attention",cwd:"/y",updated_at:2}' \
    > "$XDG_STATE_HOME/sketchybar/sessions/b.json"

  "$PLUGIN_BIN/claude-render-sessions"

  assert_sketchybar_logged \
    '--set claude_sessions .*label=2' "label=2"
  # yellow 0xfff9e2af
  assert_sketchybar_logged \
    '--set claude_sessions .*label\.color=0xfff9e2af' "yellow color"
}

test_renderer_aggregate_state_green() {
  # only running sessions → parent label color is green
  mkdir -p "$XDG_STATE_HOME/sketchybar/sessions"
  jq -n '{session_id:"a",workspace:"3",state:"running",cwd:"/x",updated_at:1}' \
    > "$XDG_STATE_HOME/sketchybar/sessions/a.json"

  "$PLUGIN_BIN/claude-render-sessions"

  assert_sketchybar_logged \
    '--set claude_sessions .*label=1' "label=1"
  # green 0xffa6e3a1
  assert_sketchybar_logged \
    '--set claude_sessions .*label\.color=0xffa6e3a1' "green color"
}

test_renderer_popup_children_sorted() {
  mkdir -p "$XDG_STATE_HOME/sketchybar/sessions"
  jq -n '{session_id:"aaaa1111zzzz",workspace:"5",state:"running",
         cwd:"/Users/me/rum-ios",updated_at:100}' \
    > "$XDG_STATE_HOME/sketchybar/sessions/aaaa1111zzzz.json"
  jq -n '{session_id:"bbbb2222zzzz",workspace:"3",state:"needs-attention",
         cwd:"/Users/me/.dotfiles",updated_at:200}' \
    > "$XDG_STATE_HOME/sketchybar/sessions/bbbb2222zzzz.json"
  jq -n '{session_id:"cccc3333zzzz",workspace:"7",state:"idle",
         cwd:"/Users/me/notes",updated_at:300}' \
    > "$XDG_STATE_HOME/sketchybar/sessions/cccc3333zzzz.json"

  "$PLUGIN_BIN/claude-render-sessions"

  # Existing children removed before re-adding (no-op when none exist,
  # but the renderer must at least query for them).
  assert_sketchybar_logged '--query bar' "query bar to enumerate children"

  # All three children added with popup.claude_sessions.
  assert_sketchybar_logged \
    '--add item claude_sessions\.bbbb2222 popup\.claude_sessions' "ws3 child"
  assert_sketchybar_logged \
    '--add item claude_sessions\.aaaa1111 popup\.claude_sessions' "ws5 child"
  assert_sketchybar_logged \
    '--add item claude_sessions\.cccc3333 popup\.claude_sessions' "ws7 child"

  # Click scripts switch workspace and toggle popup off.
  assert_sketchybar_logged \
    "--set claude_sessions\\.bbbb2222 .*click_script=.*aerospace workspace 3" \
    "ws3 click"
  assert_sketchybar_logged \
    "--set claude_sessions\\.bbbb2222 .*popup\\.drawing=off" \
    "ws3 closes popup"

  # Sort order: running (a) before needs-attention (b) before idle (c).
  # Verify by line numbers in the log.
  local b_line a_line c_line
  b_line=$(grep -n 'add item claude_sessions\.bbbb2222' "$SKETCHYBAR_LOG" | head -1 | cut -d: -f1)
  a_line=$(grep -n 'add item claude_sessions\.aaaa1111' "$SKETCHYBAR_LOG" | head -1 | cut -d: -f1)
  c_line=$(grep -n 'add item claude_sessions\.cccc3333' "$SKETCHYBAR_LOG" | head -1 | cut -d: -f1)
  [ "$a_line" -lt "$b_line" ] || fail "running should sort before needs-attention"
  [ "$b_line" -lt "$c_line" ] || fail "needs-attention should sort before idle"
}

test_renderer_emits_header_and_section_dividers() {
  mkdir -p "$XDG_STATE_HOME/sketchybar/sessions"
  jq -n '{session_id:"aaaa",workspace:"3",state:"running",cwd:"/x",updated_at:1}' \
    > "$XDG_STATE_HOME/sketchybar/sessions/aaaa.json"
  jq -n '{session_id:"bbbb",workspace:"5",state:"needs-attention",cwd:"/y",updated_at:2}' \
    > "$XDG_STATE_HOME/sketchybar/sessions/bbbb.json"

  "$PLUGIN_BIN/claude-render-sessions"

  # Header item summarising counts.
  assert_sketchybar_logged \
    '--add item claude_sessions\._header popup\.claude_sessions' "header item added"
  assert_sketchybar_logged \
    'claude_sessions\._header .*1 working . 1 waiting . 0 idle' "summary text"

  # One section divider per non-empty group (running + needs-attention here).
  assert_sketchybar_logged \
    '--add item claude_sessions\._sec_1 popup\.claude_sessions' "first section"
  assert_sketchybar_logged \
    'claude_sessions\._sec_1 .*label=WORKING' "first section label"
  assert_sketchybar_logged \
    '--add item claude_sessions\._sec_2 popup\.claude_sessions' "second section"
  assert_sketchybar_logged \
    'claude_sessions\._sec_2 .*label=WAITING' "second section label"
}

test_renderer_removes_existing_children() {
  mkdir -p "$XDG_STATE_HOME/sketchybar/sessions"
  # Mock sketchybar --query bar to report two existing children + the
  # parent + an unrelated item. Replace the previous mock for this test.
  cat > "$MOCK_BIN/sketchybar" <<EOF
#!/usr/bin/env bash
printf '%s\n' "\$*" >> "$SKETCHYBAR_LOG"
if [ "\$1" = "--query" ] && [ "\$2" = "bar" ]; then
  cat <<'JSON'
{ "items": ["claude_sessions", "claude_sessions.olda", "claude_sessions.oldb", "clock"] }
JSON
fi
exit 0
EOF
  chmod +x "$MOCK_BIN/sketchybar"

  "$PLUGIN_BIN/claude-render-sessions"

  # Both old children must be removed; parent and unrelated must not.
  assert_sketchybar_logged \
    '--remove claude_sessions\.olda' "removes olda"
  assert_sketchybar_logged \
    '--remove claude_sessions\.oldb' "removes oldb"
  if grep -Eq -- '--remove claude_sessions(\s|$)' "$SKETCHYBAR_LOG"; then
    fail "must not remove the parent claude_sessions item"
  fi
  if grep -Eq -- '--remove clock' "$SKETCHYBAR_LOG"; then
    fail "must not remove unrelated items"
  fi
}

TESTS=(
  test_helper_writes_json_with_cwd
  test_helper_purges_legacy_text_pins
  test_helper_clear_removes_session_json
  test_helper_writes_helper_log
  test_renderer_zero_sessions_dim_zero
  test_renderer_aggregate_state_yellow
  test_renderer_aggregate_state_green
  test_renderer_popup_children_sorted
  test_renderer_removes_existing_children
  test_renderer_emits_header_and_section_dividers
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
