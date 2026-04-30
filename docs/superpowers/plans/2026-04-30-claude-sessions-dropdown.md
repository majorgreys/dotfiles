# Claude sessions dropdown — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a sketchybar item left of the existing right-side status group that displays a count of active Claude Code sessions and, on click, opens a popup listing each session with its workspace, project basename, and state (running / needs-attention / idle), where clicking a row switches AeroSpace to that workspace.

**Architecture:** Per-session state moves from a text-only pin file to a JSON record at `$XDG_STATE_HOME/sketchybar/sessions/<session_id>.json`. The existing `sketchybar-set-state` helper (run from Claude Code hooks) writes those records; a new `claude-render-sessions` script enumerates them and rebuilds a popup-bearing `claude_sessions` sketchybar item. Both are wired through the existing `claude_agent_state_change` event so workspace pills and the dropdown render off the same trigger.

**Tech Stack:** bash, jq, sketchybar, AeroSpace, Claude Code plugin hooks, GNU stow.

---

## Reference paths

Throughout the plan:

- **Plugin root:** `~/.dotfiles/claude-status/.claude/plugins/local/claude-status/`
- **Helper bin dir:** `~/.dotfiles/claude-status/.claude/plugins/local/claude-status/bin/`
- **sketchybar drop-in:** `~/.dotfiles/claude-status/.config/sketchybar/plugins.d/claude.sh`
- **Local bin symlink dir (in stow tree):** `~/.dotfiles/claude-status/.local/bin/`
- **Tests dir (new):** `~/.dotfiles/claude-status/tests/`

After stow, those land at `~/.claude/plugins/local/claude-status/`, `~/.config/sketchybar/plugins.d/claude.sh`, `~/.local/bin/`. Tests are not stowed — they live only in the dotfiles tree.

## File map

**New:**

- `claude-status/.claude/plugins/local/claude-status/bin/claude-render-sessions` — enumerates `$STATE_ROOT/sessions/*.json`, updates the `claude_sessions` sketchybar item label/color, and rebuilds its popup children.
- `claude-status/.local/bin/claude-render-sessions` — relative symlink to the above so the renderer is on `PATH`.
- `claude-status/tests/test-helpers.sh` — test scaffold: tmp `XDG_STATE_HOME`, mock `aerospace` and `sketchybar` binaries on `PATH`, assertion helpers.
- `claude-status/tests/smoke.sh` — runs the suite. Each "test_*" function is one scenario.

**Modified:**

- `claude-status/.claude/plugins/local/claude-status/bin/sketchybar-set-state` — write per-session JSON instead of a bare text pin file; one-shot purge of legacy bare files; remove JSON on `clear`.
- `claude-status/.config/sketchybar/plugins.d/claude.sh` — add the `claude_sessions` parent item, popup styling, subscribe to `claude_agent_state_change`, point its `script` at `~/.local/bin/claude-render-sessions`.

**Unchanged (verified after rework):**

- `claude-status/.claude/plugins/local/claude-status/hooks/hooks.json`
- `claude-status/install.sh` / `uninstall.sh`
- `sketchybar/.config/sketchybar/sketchybarrc`
- `sketchybar/.config/sketchybar/plugins/aerospace.sh`

---

## Task 1: Test scaffold

**Files:**
- Create: `claude-status/tests/test-helpers.sh`
- Create: `claude-status/tests/smoke.sh`

The helper script (`sketchybar-set-state`) talks to `aerospace`, the filesystem, and `sketchybar`. The renderer (`claude-render-sessions`) talks to the filesystem and `sketchybar`. Both are testable headlessly by mocking `aerospace` and `sketchybar` on `PATH` and pointing `XDG_STATE_HOME` at a tempdir. This task lays down that scaffold; subsequent tasks add `test_*` functions.

- [ ] **Step 1: Create the helpers file**

Write `claude-status/tests/test-helpers.sh`:

```bash
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
  # MOCK_AEROSPACE_WS before calling setup.
  : "\${MOCK_AEROSPACE_WS:=3}"
  cat > "$MOCK_BIN/aerospace" <<EOF
#!/usr/bin/env bash
if [ "\$1" = "list-workspaces" ] && [ "\${2:-}" = "--focused" ]; then
  echo "\${MOCK_AEROSPACE_WS:-3}"
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
  if ! grep -Eq "$1" "$SKETCHYBAR_LOG"; then
    echo "sketchybar log was:" >&2
    cat "$SKETCHYBAR_LOG" >&2
    fail "${2:-assert_sketchybar_logged}: no line matched /$1/"
  fi
}
```

- [ ] **Step 2: Create the test runner**

Write `claude-status/tests/smoke.sh`:

```bash
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

TESTS=(
  # filled in as tasks land
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
```

- [ ] **Step 3: Make both executable and run**

```bash
chmod +x ~/.dotfiles/claude-status/tests/test-helpers.sh \
         ~/.dotfiles/claude-status/tests/smoke.sh
~/.dotfiles/claude-status/tests/smoke.sh
```

Expected output: `all tests passed` (with no `==>` lines, since `TESTS` is empty).

- [ ] **Step 4: Commit**

```bash
cd ~/.dotfiles
git add claude-status/tests/
git commit -m "Add smoke-test scaffold for claude-status helpers"
```

---

## Task 2: Helper writes per-session JSON

**Files:**
- Modify: `claude-status/.claude/plugins/local/claude-status/bin/sketchybar-set-state`
- Modify: `claude-status/tests/smoke.sh` (append test)

Replace the bare text pin file `$STATE_ROOT/sessions/<session_id>` with a JSON record `<session_id>.json` containing `session_id`, `workspace`, `state`, `cwd`, `updated_at`. The workspace-pinning logic (read existing pin if present; otherwise pin to focused workspace) carries over but reads/writes JSON via `jq`.

- [ ] **Step 1: Add a failing test**

Append to `claude-status/tests/smoke.sh` (above the `TESTS=(` array):

```bash
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
```

Add `test_helper_writes_json_with_cwd` to the `TESTS=(` array.

- [ ] **Step 2: Run to confirm it fails**

```bash
~/.dotfiles/claude-status/tests/smoke.sh
```

Expected: `FAIL` because the helper currently writes the bare text pin `sessions/abc12345`, not `sessions/abc12345.json`.

- [ ] **Step 3: Update the helper**

Overwrite `claude-status/.claude/plugins/local/claude-status/bin/sketchybar-set-state` with:

```bash
#!/usr/bin/env bash
# sketchybar-set-state — write per-workspace and per-session agent state.
#
# Usage:
#   sketchybar-set-state <state>
#     state: running | needs-attention | idle | clear
#     stdin (optional): Claude Code hook JSON; .session_id and .cwd are read.
#
# Behavior:
#   1. Read .session_id and .cwd from stdin JSON if present.
#   2. Resolve the session's workspace: if a session JSON record exists
#      under $STATE_ROOT/sessions/<id>.json, reuse its workspace; else
#      query AeroSpace for the focused workspace and pin to that.
#   3. Write per-workspace state to $STATE_ROOT/agents/<workspace>.
#   4. Write per-session JSON to $STATE_ROOT/sessions/<id>.json
#      (atomically: tmp + rename).
#   5. On state=clear, remove both the per-workspace and per-session
#      records.
#   6. Trigger claude_agent_state_change so subscribers re-render.

set -euo pipefail

STATE="${1:?state required: running|needs-attention|idle|clear}"

STATE_ROOT="${XDG_STATE_HOME:-$HOME/.local/state}/sketchybar"
SESS_DIR="$STATE_ROOT/sessions"
WS_DIR="$STATE_ROOT/agents"
mkdir -p "$SESS_DIR" "$WS_DIR"

HOOK_INPUT=""
if [ ! -t 0 ]; then
  HOOK_INPUT=$(cat || true)
fi

SESSION_ID=""
CWD=""
if [ -n "$HOOK_INPUT" ] && command -v jq >/dev/null 2>&1; then
  SESSION_ID=$(printf '%s' "$HOOK_INPUT" | jq -r '.session_id // empty' 2>/dev/null || true)
  CWD=$(printf '%s' "$HOOK_INPUT" | jq -r '.cwd // empty' 2>/dev/null || true)
fi

# Resolve the pinned workspace for this session.
WS=""
SESS_FILE=""
if [ -n "$SESSION_ID" ]; then
  SESS_FILE="$SESS_DIR/$SESSION_ID.json"
  if [ -s "$SESS_FILE" ]; then
    WS=$(jq -r '.workspace // empty' "$SESS_FILE" 2>/dev/null || true)
  fi
fi
if [ -z "$WS" ]; then
  WS=$(aerospace list-workspaces --focused 2>/dev/null || true)
fi
[ -z "$WS" ] && exit 0

if [ "$STATE" = "clear" ]; then
  rm -f "$WS_DIR/$WS"
  [ -n "$SESS_FILE" ] && rm -f "$SESS_FILE"
else
  printf '%s' "$STATE" > "$WS_DIR/$WS"

  if [ -n "$SESSION_ID" ]; then
    NOW=$(date +%s)
    TMP="$SESS_FILE.tmp"
    jq -n \
      --arg sid "$SESSION_ID" \
      --arg ws  "$WS" \
      --arg st  "$STATE" \
      --arg cw  "$CWD" \
      --argjson ts "$NOW" \
      '{session_id:$sid, workspace:$ws, state:$st, cwd:$cw, updated_at:$ts}' \
      > "$TMP"
    mv "$TMP" "$SESS_FILE"
  fi
fi

sketchybar --trigger claude_agent_state_change >/dev/null 2>&1 || true
exit 0
```

- [ ] **Step 4: Run to confirm it passes**

```bash
~/.dotfiles/claude-status/tests/smoke.sh
```

Expected: `all tests passed`.

- [ ] **Step 5: Counterfactual — break each assertion to confirm it fires**

Temporarily change one assertion at a time and re-run to confirm the test fails. For example, change `assert_eq "$(jq -r .cwd "$f")" "/Users/me/proj-one"` to `"/wrong"` and verify the test reports the failure. Restore after.

- [ ] **Step 6: Commit**

```bash
cd ~/.dotfiles
git add claude-status/.claude/plugins/local/claude-status/bin/sketchybar-set-state \
        claude-status/tests/smoke.sh
git commit -m "Write per-session JSON with cwd in sketchybar-set-state"
```

---

## Task 3: Helper purges legacy text pin files

**Files:**
- Modify: `claude-status/.claude/plugins/local/claude-status/bin/sketchybar-set-state`
- Modify: `claude-status/tests/smoke.sh`

Anyone who installed the previous version has bare text pin files (e.g. `sessions/abc12345` containing `3`). The renderer ignores non-`.json` files, but those bare files would otherwise linger forever. One-shot purge at helper entry.

- [ ] **Step 1: Add a failing test**

Append to `claude-status/tests/smoke.sh` (above `TESTS=(`):

```bash
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
```

Add `test_helper_purges_legacy_text_pins` to the `TESTS=(` array.

- [ ] **Step 2: Run to confirm it fails**

```bash
~/.dotfiles/claude-status/tests/smoke.sh
```

Expected: `FAIL: expected file '.../sessions/legacy-id' to not exist`.

- [ ] **Step 3: Add purge to the helper**

In `sketchybar-set-state`, after the `mkdir -p "$SESS_DIR" "$WS_DIR"` line, insert:

```bash
# One-shot purge: drop legacy bare-text pin files from earlier versions
# of this helper. We only keep .json records now.
for f in "$SESS_DIR"/*; do
  [ -e "$f" ] || continue
  case "$f" in
    *.json|*.tmp) ;;
    *) rm -f "$f" ;;
  esac
done
```

- [ ] **Step 4: Run to confirm it passes**

```bash
~/.dotfiles/claude-status/tests/smoke.sh
```

Expected: `all tests passed`.

- [ ] **Step 5: Commit**

```bash
cd ~/.dotfiles
git add claude-status/.claude/plugins/local/claude-status/bin/sketchybar-set-state \
        claude-status/tests/smoke.sh
git commit -m "Purge legacy text pin files in sketchybar-set-state"
```

---

## Task 4: Helper removes JSON on clear

**Files:**
- Modify: `claude-status/tests/smoke.sh`

The helper already implements `clear` by removing both the per-workspace file and the per-session JSON (verified by reading the new helper from Task 2). This task adds a regression test so the behavior is locked in.

- [ ] **Step 1: Add a test**

Append to `claude-status/tests/smoke.sh` (above `TESTS=(`):

```bash
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
```

Add `test_helper_clear_removes_session_json` to the `TESTS=(` array.

- [ ] **Step 2: Run to confirm it passes**

```bash
~/.dotfiles/claude-status/tests/smoke.sh
```

Expected: `all tests passed`.

- [ ] **Step 3: Counterfactual**

Temporarily comment out `[ -n "$SESS_FILE" ] && rm -f "$SESS_FILE"` in the helper and re-run. The test should report a failure on the JSON-still-exists assertion. Restore the line.

- [ ] **Step 4: Commit**

```bash
cd ~/.dotfiles
git add claude-status/tests/smoke.sh
git commit -m "Test: sketchybar-set-state clear removes per-session JSON"
```

---

## Task 5: Renderer — parent label and color

**Files:**
- Create: `claude-status/.claude/plugins/local/claude-status/bin/claude-render-sessions`
- Modify: `claude-status/tests/smoke.sh`

The renderer's first responsibility: count session JSON files in `$STATE_ROOT/sessions/` and update the parent item's label (count) and label color (red if any `needs-attention`, yellow else if any `running`, dim grey if 0). Popup children come in Task 6.

- [ ] **Step 1: Add a failing test for the empty case**

Append to `claude-status/tests/smoke.sh`:

```bash
test_renderer_zero_sessions_dim_zero() {
  "$PLUGIN_BIN/claude-render-sessions"

  # Parent label set to "0", color = dim grey 0xff7f849c
  assert_sketchybar_logged \
    '--set claude_sessions .*label=0' "label=0"
  assert_sketchybar_logged \
    '--set claude_sessions .*label\.color=0xff7f849c' "dim grey"
}

test_renderer_aggregate_state_red() {
  mkdir -p "$XDG_STATE_HOME/sketchybar/sessions"
  jq -n '{session_id:"a",workspace:"3",state:"running",cwd:"/x",updated_at:1}' \
    > "$XDG_STATE_HOME/sketchybar/sessions/a.json"
  jq -n '{session_id:"b",workspace:"5",state:"needs-attention",cwd:"/y",updated_at:2}' \
    > "$XDG_STATE_HOME/sketchybar/sessions/b.json"

  "$PLUGIN_BIN/claude-render-sessions"

  assert_sketchybar_logged \
    '--set claude_sessions .*label=2' "label=2"
  # red 0xfff38ba8
  assert_sketchybar_logged \
    '--set claude_sessions .*label\.color=0xfff38ba8' "red color"
}

test_renderer_aggregate_state_yellow() {
  mkdir -p "$XDG_STATE_HOME/sketchybar/sessions"
  jq -n '{session_id:"a",workspace:"3",state:"running",cwd:"/x",updated_at:1}' \
    > "$XDG_STATE_HOME/sketchybar/sessions/a.json"

  "$PLUGIN_BIN/claude-render-sessions"

  assert_sketchybar_logged \
    '--set claude_sessions .*label=1' "label=1"
  # yellow 0xfff9e2af
  assert_sketchybar_logged \
    '--set claude_sessions .*label\.color=0xfff9e2af' "yellow color"
}
```

Add the three function names to `TESTS=(` in order.

- [ ] **Step 2: Run to confirm they fail**

```bash
~/.dotfiles/claude-status/tests/smoke.sh
```

Expected: `FAIL` because `claude-render-sessions` does not exist.

- [ ] **Step 3: Create the renderer**

Create `claude-status/.claude/plugins/local/claude-status/bin/claude-render-sessions`:

```bash
#!/usr/bin/env bash
# claude-render-sessions — repaint the sketchybar `claude_sessions`
# parent item and rebuild its popup children from per-session JSON
# records under $XDG_STATE_HOME/sketchybar/sessions/.
#
# Invoked as the `script` of the claude_sessions sketchybar item, so it
# runs on initial render and on every claude_agent_state_change.
#
# Output: only sketchybar --set / --add / --remove commands; nothing on
# stdout/stderr in normal operation.

set -euo pipefail

STATE_ROOT="${XDG_STATE_HOME:-$HOME/.local/state}/sketchybar"
SESS_DIR="$STATE_ROOT/sessions"

# Aggregate state and count.
COUNT=0
HAS_NEEDS=0
HAS_RUN=0
shopt -s nullglob
for f in "$SESS_DIR"/*.json; do
  COUNT=$((COUNT + 1))
  STATE=$(jq -r '.state // empty' "$f" 2>/dev/null || true)
  case "$STATE" in
    needs-attention) HAS_NEEDS=1 ;;
    running)         HAS_RUN=1 ;;
  esac
done
shopt -u nullglob

# Catppuccin palette (matches sketchybarrc + workspace-pill plugin).
COLOR_FG="0xffcdd6f4"
COLOR_DIM="0xff7f849c"
COLOR_YELLOW="0xfff9e2af"
COLOR_RED="0xfff38ba8"

if [ "$COUNT" -eq 0 ]; then
  PARENT_COLOR="$COLOR_DIM"
elif [ "$HAS_NEEDS" -eq 1 ]; then
  PARENT_COLOR="$COLOR_RED"
elif [ "$HAS_RUN" -eq 1 ]; then
  PARENT_COLOR="$COLOR_YELLOW"
else
  PARENT_COLOR="$COLOR_FG"
fi

sketchybar --set claude_sessions \
  label="$COUNT" \
  label.color="$PARENT_COLOR"
```

- [ ] **Step 4: Make executable and run**

```bash
chmod +x ~/.dotfiles/claude-status/.claude/plugins/local/claude-status/bin/claude-render-sessions
~/.dotfiles/claude-status/tests/smoke.sh
```

Expected: `all tests passed`.

- [ ] **Step 5: Counterfactual**

Temporarily change `PARENT_COLOR="$COLOR_RED"` to `PARENT_COLOR="$COLOR_FG"` and re-run. The red-aggregate test should fail. Restore.

- [ ] **Step 6: Commit**

```bash
cd ~/.dotfiles
git add claude-status/.claude/plugins/local/claude-status/bin/claude-render-sessions \
        claude-status/tests/smoke.sh
git commit -m "Add claude-render-sessions: parent label/color from session JSON"
```

---

## Task 6: Renderer — popup children, sort, click scripts

**Files:**
- Modify: `claude-status/.claude/plugins/local/claude-status/bin/claude-render-sessions`
- Modify: `claude-status/tests/smoke.sh`

Rebuild popup children on each render: enumerate, sort (`needs-attention` → `running` → `idle`, then by workspace ascending, then by `updated_at` descending), wipe existing children whose names start with `claude_sessions.`, then add one popup child per session with its dot color, label, and click script.

- [ ] **Step 1: Add tests**

Append to `claude-status/tests/smoke.sh`:

```bash
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

  # Sort order: needs-attention (b) before running (a) before idle (c).
  # Verify by line numbers in the log.
  local b_line a_line c_line
  b_line=$(grep -n 'add item claude_sessions\.bbbb2222' "$SKETCHYBAR_LOG" | head -1 | cut -d: -f1)
  a_line=$(grep -n 'add item claude_sessions\.aaaa1111' "$SKETCHYBAR_LOG" | head -1 | cut -d: -f1)
  c_line=$(grep -n 'add item claude_sessions\.cccc3333' "$SKETCHYBAR_LOG" | head -1 | cut -d: -f1)
  [ "$b_line" -lt "$a_line" ] || fail "needs-attention should sort before running"
  [ "$a_line" -lt "$c_line" ] || fail "running should sort before idle"
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
```

Add `test_renderer_popup_children_sorted` and `test_renderer_removes_existing_children` to `TESTS=(`.

- [ ] **Step 2: Run to confirm they fail**

```bash
~/.dotfiles/claude-status/tests/smoke.sh
```

Expected: `FAIL` — current renderer only handles parent.

- [ ] **Step 3: Extend the renderer**

Replace `claude-status/.claude/plugins/local/claude-status/bin/claude-render-sessions` with:

```bash
#!/usr/bin/env bash
# claude-render-sessions — repaint the sketchybar `claude_sessions`
# parent item and rebuild its popup children from per-session JSON
# records under $XDG_STATE_HOME/sketchybar/sessions/.
#
# Invoked as the `script` of the claude_sessions sketchybar item, so it
# runs on initial render and on every claude_agent_state_change.

set -euo pipefail

STATE_ROOT="${XDG_STATE_HOME:-$HOME/.local/state}/sketchybar"
SESS_DIR="$STATE_ROOT/sessions"

# Catppuccin palette (matches sketchybarrc + workspace-pill plugin).
COLOR_FG="0xffcdd6f4"
COLOR_DIM="0xff7f849c"
COLOR_YELLOW="0xfff9e2af"
COLOR_RED="0xfff38ba8"

# Read all session JSONs into a single jq input array. Each entry gets
# a sort_state numeric for the primary sort key.
sessions_json() {
  shopt -s nullglob
  local files=("$SESS_DIR"/*.json)
  shopt -u nullglob
  if [ "${#files[@]}" -eq 0 ]; then
    echo '[]'
    return
  fi
  jq -s 'map(select(.session_id and .workspace and .state))' "${files[@]}"
}

ALL=$(sessions_json)
COUNT=$(jq 'length' <<< "$ALL")

HAS_NEEDS=$(jq '[.[] | select(.state == "needs-attention")] | length' <<< "$ALL")
HAS_RUN=$(jq '[.[] | select(.state == "running")]         | length' <<< "$ALL")

if [ "$COUNT" -eq 0 ]; then
  PARENT_COLOR="$COLOR_DIM"
elif [ "$HAS_NEEDS" -gt 0 ]; then
  PARENT_COLOR="$COLOR_RED"
elif [ "$HAS_RUN" -gt 0 ]; then
  PARENT_COLOR="$COLOR_YELLOW"
else
  PARENT_COLOR="$COLOR_FG"
fi

sketchybar --set claude_sessions \
  label="$COUNT" \
  label.color="$PARENT_COLOR"

# Wipe any existing popup children. Query the bar, filter for items
# whose names start with "claude_sessions." (note the dot — must not
# remove the parent itself).
EXISTING=$(sketchybar --query bar 2>/dev/null \
  | jq -r '.items[]? | select(startswith("claude_sessions."))' \
  || true)
if [ -n "$EXISTING" ]; then
  while IFS= read -r name; do
    [ -n "$name" ] && sketchybar --remove "$name"
  done <<< "$EXISTING"
fi

# Sort and emit one child per session. Sort key:
#   needs-attention=0, running=1, idle=2, other=3
#   then workspace asc (numeric)
#   then updated_at desc
SORTED=$(jq -c '
  def state_rank:
    if . == "needs-attention" then 0
    elif . == "running" then 1
    elif . == "idle" then 2
    else 3 end;
  sort_by(
    (.state | state_rank),
    (.workspace | tonumber? // 999),
    -(.updated_at)
  ) | .[]
' <<< "$ALL")

# Compute longest project basename for column alignment in mono font.
MAX_PROJ=0
while IFS= read -r row; do
  [ -z "$row" ] && continue
  PROJ=$(jq -r '.cwd // "" | split("/") | last // "?"' <<< "$row")
  [ -z "$PROJ" ] && PROJ="?"
  L=${#PROJ}
  [ "$L" -gt "$MAX_PROJ" ] && MAX_PROJ=$L
done <<< "$SORTED"

while IFS= read -r row; do
  [ -z "$row" ] && continue
  SID=$(jq -r .session_id  <<< "$row")
  WS=$(jq  -r .workspace   <<< "$row")
  ST=$(jq  -r .state       <<< "$row")
  CWD=$(jq -r '.cwd // ""' <<< "$row")

  PROJ=$(printf '%s' "$CWD" | awk -F/ '{print $NF}')
  [ -z "$PROJ" ] && PROJ="?"
  # Pad PROJ to MAX_PROJ for column alignment (mono font).
  while [ "${#PROJ}" -lt "$MAX_PROJ" ]; do PROJ="$PROJ "; done

  case "$ST" in
    running)         DOT_COLOR="$COLOR_YELLOW" ;;
    needs-attention) DOT_COLOR="$COLOR_RED" ;;
    idle)            DOT_COLOR="$COLOR_DIM" ;;
    *)               DOT_COLOR="$COLOR_FG" ;;
  esac

  SHORT="${SID:0:8}"
  CHILD="claude_sessions.$SHORT"

  LABEL=$(printf '%s  %s  %s' "$WS" "$PROJ" "$ST")

  sketchybar --add item "$CHILD" popup.claude_sessions \
             --set "$CHILD" \
                   icon="●" \
                   icon.color="$DOT_COLOR" \
                   icon.padding_left=8 \
                   icon.padding_right=6 \
                   label="$LABEL" \
                   label.color="$COLOR_FG" \
                   label.padding_right=8 \
                   click_script="aerospace workspace $WS && sketchybar --set claude_sessions popup.drawing=off"
done <<< "$SORTED"
```

- [ ] **Step 4: Run to confirm tests pass**

```bash
~/.dotfiles/claude-status/tests/smoke.sh
```

Expected: `all tests passed`.

- [ ] **Step 5: Counterfactual**

Temporarily flip the sort order: change `(.state | state_rank)` to `-(.state | state_rank)` in the jq sort. The sort-order assertion in `test_renderer_popup_children_sorted` should fail. Restore.

- [ ] **Step 6: Commit**

```bash
cd ~/.dotfiles
git add claude-status/.claude/plugins/local/claude-status/bin/claude-render-sessions \
        claude-status/tests/smoke.sh
git commit -m "Render claude_sessions popup children sorted by state and workspace"
```

---

## Task 7: Wire the parent item in claude.sh

**Files:**
- Modify: `claude-status/.config/sketchybar/plugins.d/claude.sh`

The drop-in is sourced by `~/.dotfiles/sketchybar/.config/sketchybar/sketchybarrc` after the four right-side items (`clock battery volume wifi`) have been added. Sketchybar adds new right-side items to the *left* of existing right-side items, so a fresh `--add item claude_sessions right` lands in the leftmost slot of the status group — exactly where we want it. No reordering required.

This task is not unit-tested (it manipulates a live sketchybar). Verification is visual + by inspecting `sketchybar --query`.

- [ ] **Step 1: Edit `claude.sh`**

Overwrite `claude-status/.config/sketchybar/plugins.d/claude.sh` with:

```bash
# claude-status sketchybar plugin
#
# Sourced by the base sketchybar config (see ~/.dotfiles/sketchybar/sketchybarrc)
# AFTER the right-side status items (clock, battery, volume, wifi) and the
# workspace pills have been added.
#
# Provides:
#   1. The claude_agent_state_change event.
#   2. Workspace-pill subscriptions so each pill repaints when state changes.
#   3. The `claude_sessions` parent item (status-group leftmost) plus its
#      popup; clicking the pill toggles the popup, and a renderer script
#      rebuilds children on every event tick.
#
# State contracts (read by both sides):
#   $XDG_STATE_HOME/sketchybar/agents/<workspace>      one of:
#     running | needs-attention | idle | (missing/empty for no dot)
#   $XDG_STATE_HOME/sketchybar/sessions/<id>.json      JSON record per
#     active session: {session_id, workspace, state, cwd, updated_at}.

sketchybar --add event claude_agent_state_change

for i in 1 2 3 4 5 6 7 8 9 10; do
  sketchybar --subscribe space.$i claude_agent_state_change
done

# Catppuccin colors (must match the workspace-pill plugin).
CLAUDE_COLOR_FG="0xffcdd6f4"
CLAUDE_COLOR_DIM="0xff7f849c"
CLAUDE_COLOR_ACCENT="0xff89b4fa"
CLAUDE_COLOR_BG="0xff000000"
CLAUDE_FONT_ICON="FiraCode Nerd Font Mono:Regular:15.0"
CLAUDE_FONT_REGULAR="PragmataPro Mono Liga:Regular:13.0"

# Parent item — added with `right` after the four status items, so it
# lands as the leftmost element of the right-side status group:
#   ... | claude_sessions | wifi | volume | battery | clock |
sketchybar --add item claude_sessions right \
           --set claude_sessions \
                 icon="󰚩" \
                 icon.font="$CLAUDE_FONT_ICON" \
                 icon.color="$CLAUDE_COLOR_FG" \
                 icon.padding_left=8 \
                 icon.padding_right=6 \
                 label="0" \
                 label.font="$CLAUDE_FONT_REGULAR" \
                 label.color="$CLAUDE_COLOR_DIM" \
                 label.padding_right=8 \
                 click_script="sketchybar --set claude_sessions popup.drawing=toggle" \
                 popup.background.color="$CLAUDE_COLOR_BG" \
                 popup.background.corner_radius=6 \
                 popup.background.border_width=1 \
                 popup.background.border_color="$CLAUDE_COLOR_ACCENT" \
                 popup.horizontal=off \
                 popup.align=right \
                 script="$HOME/.local/bin/claude-render-sessions" \
           --subscribe claude_sessions claude_agent_state_change
```

- [ ] **Step 2: Reload sketchybar**

```bash
sketchybar --reload
```

- [ ] **Step 3: Verify the parent item exists and is in the right place**

```bash
sketchybar --query bar | jq -r '.items[]'
```

Expected output ends with the right-side group in this order (top of list to bottom = left to right on screen):

```
…
space.10
claude_sessions
wifi
volume
battery
clock
```

`claude_sessions` must appear immediately before `wifi`. If it appears anywhere else, the drop-in is being sourced before the right items are added — investigate `sketchybarrc` ordering.

- [ ] **Step 4: Verify the pill renders with count 0**

Visual check:

- The `󰚩` glyph appears followed by `0` to the left of `wifi`.
- `0` is dim grey.

If the `󰚩` glyph renders as a tofu box, `FiraCode Nerd Font Mono` is not the right name on this system; verify with `sketchybar --query battery | jq .icon.font`. If the issue is the glyph itself (some Nerd Font versions lack `nf-md-robot`), swap to one of the alternatives noted in the spec by editing the `icon=` line: ``  (`nf-cod-copilot`), `󰧑` (`nf-md-head_cog`), `󱙺` (`nf-md-robot_outline`).

- [ ] **Step 5: Click the pill**

Clicking should toggle a popup. With zero sessions the popup is empty (just a thin bordered box). Click again to close.

- [ ] **Step 6: Commit**

```bash
cd ~/.dotfiles
git add claude-status/.config/sketchybar/plugins.d/claude.sh
git commit -m "Add claude_sessions popup item to sketchybar drop-in"
```

---

## Task 8: Symlink claude-render-sessions onto PATH

**Files:**
- Create: `claude-status/.local/bin/claude-render-sessions` (relative symlink)

The drop-in sets `script="$HOME/.local/bin/claude-render-sessions"`. This task creates the stow symlink so that path resolves. The companion symlink for `sketchybar-set-state` already exists in the same directory.

- [ ] **Step 1: Confirm the existing symlink shape**

```bash
ls -l ~/.dotfiles/claude-status/.local/bin/sketchybar-set-state
```

Expected: a relative symlink to `../../.claude/plugins/local/claude-status/bin/sketchybar-set-state`.

- [ ] **Step 2: Create the new symlink with the same shape**

```bash
cd ~/.dotfiles/claude-status/.local/bin
ln -sf ../../.claude/plugins/local/claude-status/bin/claude-render-sessions \
       claude-render-sessions
ls -l claude-render-sessions
```

Expected: a relative symlink to `../../.claude/plugins/local/claude-status/bin/claude-render-sessions`. Resolving the symlink (`readlink -f`) should land on the file inside the dotfiles tree.

- [ ] **Step 3: Re-stow so the symlink propagates to `~/.local/bin/`**

```bash
cd ~/.dotfiles
stow -R claude-status
ls -l ~/.local/bin/claude-render-sessions
command -v claude-render-sessions
```

Expected: `~/.local/bin/claude-render-sessions` exists and `command -v` resolves it.

- [ ] **Step 4: Reload sketchybar to pick up the now-resolvable script path**

```bash
sketchybar --reload
```

- [ ] **Step 5: Verify the renderer runs on event**

```bash
sketchybar --trigger claude_agent_state_change
sketchybar --query claude_sessions | jq '.label'
```

Expected: `"0"`. The renderer ran (no errors), saw zero session JSONs, and set the label.

- [ ] **Step 6: Commit**

```bash
cd ~/.dotfiles
git add claude-status/.local/bin/claude-render-sessions
git commit -m "Symlink claude-render-sessions into ~/.local/bin"
```

---

## Task 9: End-to-end manual smoke

**Files:** none modified.

Run the spec's manual scenario against the live bar to confirm everything wires up. Each step is a manual visual check.

- [ ] **Step 1: Two synthetic sessions**

```bash
echo '{"session_id":"t1aaaaaa","cwd":"/Users/me/p1"}' \
  | sketchybar-set-state running
echo '{"session_id":"t2bbbbbb","cwd":"/Users/me/p2"}' \
  | sketchybar-set-state needs-attention
```

Expected on the bar:

- `claude_sessions` shows `󰚩 2` with the `2` in red (because one is `needs-attention`).
- The current focused workspace pill has a yellow or red dot (it now reflects whichever state was written most recently — that's existing behaviour).

- [ ] **Step 2: Open the popup**

Click the `󰚩 2` pill. Popup shows two rows:

```
● <ws>  p2  needs-attention
● <ws>  p1  running
```

`needs-attention` row appears first (sort key).

- [ ] **Step 3: Click a row**

Click the `p1` (running) row. AeroSpace switches to that workspace and the popup closes.

- [ ] **Step 4: Clear one session**

```bash
echo '{"session_id":"t2bbbbbb"}' | sketchybar-set-state clear
```

Expected:

- `claude_sessions` shows `󰚩 1` with the `1` in yellow.
- Popup (re-open it) now shows only the `p1` running row.

- [ ] **Step 5: Clear the last session**

```bash
echo '{"session_id":"t1aaaaaa"}' | sketchybar-set-state clear
```

Expected:

- `claude_sessions` shows `󰚩 0` with the `0` in dim grey.
- Popup is empty.

- [ ] **Step 6: Confirm a real Claude Code session lights up the dropdown**

Open a Claude Code window in any project and submit a prompt. Within a second the pill should jump to `1` in yellow, and the popup row should show that workspace + the basename of the project's `cwd` + `running`. When Claude finishes turn (Stop hook), it flips to red `needs-attention`. When the session ends (close the conversation / quit Claude), the pill drops back to `0`.

No commit — verification only.
