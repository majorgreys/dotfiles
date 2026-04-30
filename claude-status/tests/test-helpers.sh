#!/usr/bin/env bash
# Test scaffold for claude-status helpers.
#
# Each test function should call setup at the top and trap teardown EXIT.
# setup creates a tmpdir, points XDG_STATE_HOME at it, and prepends a
# mock bin dir to PATH containing fake aerospace + sketchybar so the
# helpers can be exercised without a running bar.

set -euo pipefail

TESTS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PLUGIN_ROOT="$TESTS_DIR/../.claude/plugins/local/claude-status"
PLUGIN_BIN="$PLUGIN_ROOT/bin"

setup() {
  TEST_TMP="$(mktemp -d)"
  export XDG_STATE_HOME="$TEST_TMP/state"
  mkdir -p "$XDG_STATE_HOME"

  MOCK_BIN="$TEST_TMP/bin"
  mkdir -p "$MOCK_BIN"

  # Sketchybar mock: log argv to a file, exit 0.
  export SKETCHYBAR_LOG="$TEST_TMP/sketchybar.log"
  : > "$SKETCHYBAR_LOG"
  cat > "$MOCK_BIN/sketchybar" <<EOF
#!/usr/bin/env bash
printf '%s\n' "\$*" >> "$SKETCHYBAR_LOG"
exit 0
EOF

  # AeroSpace mock: only "list-workspaces --focused" is consulted.
  # Default focused workspace is "3"; tests can override via
  # MOCK_AEROSPACE_WS before calling setup. Note: the value is baked
  # into the mock script at setup time (the heredoc below uses outer-
  # shell interpolation) — changing MOCK_AEROSPACE_WS mid-test won't
  # affect the already-written mock.
  : "${MOCK_AEROSPACE_WS:=3}"
  cat > "$MOCK_BIN/aerospace" <<EOF
#!/usr/bin/env bash
if [ "\$1" = "list-workspaces" ] && [ "\${2:-}" = "--focused" ]; then
  echo "${MOCK_AEROSPACE_WS:-3}"
fi
exit 0
EOF

  chmod +x "$MOCK_BIN/sketchybar" "$MOCK_BIN/aerospace"
  export PATH="$MOCK_BIN:$PATH"
}

teardown() {
  rm -rf "$TEST_TMP"
}

# Print fail message and exit 1.
fail() {
  echo "FAIL: $*" >&2
  exit 1
}

# Assert two strings are equal. Args: actual, expected, [label]
assert_eq() {
  if [ "$1" != "$2" ]; then
    fail "${3:-assert_eq}: expected '$2', got '$1'"
  fi
}

# Assert a file exists.
assert_file() {
  [ -f "$1" ] || fail "expected file '$1' to exist"
}

# Assert a file does not exist.
assert_no_file() {
  [ ! -e "$1" ] || fail "expected file '$1' to not exist"
}

# Assert sketchybar mock log contains a line matching the regex.
# Args: regex, [label]
assert_sketchybar_logged() {
  if ! grep -Eq -e "$1" "$SKETCHYBAR_LOG"; then
    echo "sketchybar log was:" >&2
    cat "$SKETCHYBAR_LOG" >&2
    fail "${2:-assert_sketchybar_logged}: no line matched /$1/"
  fi
}
