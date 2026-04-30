# Claude sessions dropdown — design

## Goal

Add a sketchybar item that lists every active Claude Code session and its
state, openable from the menu bar. Lives alongside (not in place of) the
existing per-workspace dot pills.

## User-facing shape

**Collapsed pill** (always visible, positioned right side, leftmost of
the status group — i.e. to the left of `wifi`, so visual order
left-to-right is `… claude_sessions  wifi  volume  battery  clock`):

```
[󰚩 N]
```

- Icon: `󰚩` (Nerd Font `nf-md-robot`) rendered in `FiraCode Nerd Font
  Mono` to match the other status-group icons. Reads as "coding agent"
  without branding it Claude-specifically.

  Alternatives that fit: `` (`nf-cod-copilot`, recognisable but branded
  GitHub Copilot), `󰧑` (`nf-md-head_cog`, "thinking AI"), `󱙺`
  (`nf-md-robot_outline`, lighter-weight robot). Single-character swap
  in `claude.sh` if a different glyph reads better in the bar font.
- `N` is the count of active sessions (sessions with a record on disk).
- Count color encodes aggregate state:
  - red (`0xfff38ba8`) if any session is `needs-attention`,
  - yellow (`0xfff9e2af`) else if any is `running`,
  - dim grey (`0xff7f849c`) if `N == 0`,
  - default fg (`0xffcdd6f4`) otherwise.
- Clicking the pill toggles the popup.

**Popup row** (one per active session):

```
● <ws>  <project-basename>  <state>
```

- `●` colored by state (yellow / red / grey), same palette as workspace
  pill dots.
- `<ws>`: AeroSpace workspace number the session is pinned to.
- `<project-basename>`: `basename "$cwd"` from the hook payload.
- `<state>`: literal `running` / `needs-attention` / `idle`.

Row click runs `aerospace workspace <ws>` and closes the popup.

**Sort order:** `needs-attention` → `running` → `idle`, then by workspace
ascending, then by `updated_at` descending.

## Architecture

Two pieces wired through the existing `claude_agent_state_change` event:

1. **Per-session JSON state record** at
   `$XDG_STATE_HOME/sketchybar/sessions/<session_id>.json`, replacing the
   current text-only pin file.
2. **A new sketchybar item** `claude_sessions` with a popup, plus a
   rebuild script that re-populates the popup's children whenever the
   event fires.

The existing `agents/<workspace>` text file and the workspace-pill
rendering are left unchanged. The new feature is additive.

## Data model

`$XDG_STATE_HOME/sketchybar/sessions/<session_id>.json`:

```json
{
  "session_id": "abc123…",
  "workspace": "3",
  "state": "running",
  "cwd": "/Users/tahir.butt/.dotfiles",
  "updated_at": 1730000000
}
```

- `state` ∈ `running | needs-attention | idle`. Mirrors what is written
  to `agents/<workspace>`.
- `cwd` captured from `.cwd` in the hook payload. Updated on every
  event.
- `updated_at`: `date +%s` at write time. Used for sort tie-breaking and
  for any future stale-record reaper.
- Removed when the helper is invoked with `clear` (i.e. on `SessionEnd`).

Writes are atomic: write to `<file>.tmp`, then `mv`.

**Migration of existing pin files:** the helper performs a one-shot
purge at the top — any file under `$STATE_ROOT/sessions/` whose name
does not end in `.json` is `rm`'d. This drops the old text pins. The
rebuild script ignores non-`.json` files defensively as well.

## Components

### `sketchybar-set-state` (existing helper, modified)

Lives at
`claude-status/.claude/plugins/local/claude-status/bin/sketchybar-set-state`.

Current responsibility: pin a session to its first-seen AeroSpace
workspace, then write `<state>` to `agents/<workspace>`.

Changes:

- Read both `.session_id` and `.cwd` from the hook JSON on stdin.
- After writing `agents/<workspace>` (unchanged), also write
  `sessions/<session_id>.json` atomically with the four fields above.
- On `clear`, also `rm -f sessions/<session_id>.json`.
- At entry, perform the one-shot non-`.json` purge described above.
- Trigger `claude_agent_state_change` (already done).

Behavior with no `session_id` (manual nudges from the CLI without
piping JSON) is unchanged: the helper still writes `agents/<workspace>`
but skips the per-session JSON. The dropdown will not show those
manual nudges, which is the desired outcome (they do not represent
real Claude sessions).

### `claude-render-sessions` (new helper)

Lives at
`claude-status/.claude/plugins/local/claude-status/bin/claude-render-sessions`.

Invoked as the `script` of the `claude_sessions` sketchybar item, so
sketchybar runs it on initial render and on every
`claude_agent_state_change`.

Responsibilities:

1. List `$STATE_ROOT/sessions/*.json` (glob; tolerate empty match).
2. For each, parse `session_id`, `workspace`, `state`, `cwd`,
   `updated_at` via `jq`.
3. Compute aggregate state for the parent (`needs-attention` >
   `running` > `idle` > none) and the count.
4. Update the parent: count label, label color per the rules above.
5. Remove existing popup children: `sketchybar --query bar` lists items;
   filter for names starting with `claude_sessions.` and `--remove` each.
6. For each session in sort order, `--add item claude_sessions.<short>`
   onto `popup.claude_sessions` and `--set` its label, label colors, and
   `click_script` to switch workspace and toggle the popup off.

`<short>` is the first 8 chars of `session_id` — enough to avoid
collisions in practice while keeping sketchybar item names short.

### `claude.sh` (existing sketchybar drop-in, extended)

Lives at
`claude-status/.config/sketchybar/plugins.d/claude.sh`.

Currently registers the `claude_agent_state_change` event and
subscribes each workspace pill.

Adds:

- The `claude_sessions` parent item, positioned `right`. Because
  sketchybar adds right-side items right-to-left and the base
  `sketchybarrc` sources `plugins.d/*.sh` after the four existing
  right-side items have been added, a plain `--add item claude_sessions
  right` lands the item to the left of `wifi` — exactly the leftmost
  position in the status group, which is what we want. No reordering
  of existing items required.

- `--subscribe claude_sessions claude_agent_state_change` and
  `--set claude_sessions script="$HOME/.local/bin/claude-render-sessions"`.

  Absolute path is used rather than `command -v` because sketchybar's
  daemon may not inherit a shell `PATH` that includes `~/.local/bin`.
  The `~/.local/bin/claude-render-sessions` symlink is created by the
  stow package, so the path is stable.

- `popup.background` styling: rounded corners (radius 6), 1px border in
  accent color (`0xff89b4fa`), small padding. Matches the bar's existing
  `background.corner_radius=4` family.

### Hooks (no change)

The four hooks in `hooks/hooks.json` already cover everything:
`UserPromptSubmit` → `running`, `Stop` / `Notification` →
`needs-attention`, `SessionEnd` → `clear`. The helper now also writes
the per-session JSON as a side effect.

## Failure modes & defensiveness

- **Malformed JSON file:** rebuild script `jq`'s with `// empty` defaults
  and skips entries that fail to parse. Logs nothing; the row simply
  does not appear.
- **No `cwd` in payload:** project column shows `?`.
- **Stale records (Claude crashed without `SessionEnd`):** out of scope
  for v1. `updated_at` is captured so a future reaper can prune entries
  older than some TTL. Manual cleanup: `rm $STATE_ROOT/sessions/*.json
  && sketchybar --trigger claude_agent_state_change`.
- **Two sessions in the same workspace:** both appear as separate rows
  in the popup. The single workspace dot continues to reflect whichever
  state was written most recently — this is unchanged from today and
  intentionally not addressed here.
- **Click-to-switch on a workspace that no longer exists:** `aerospace
  workspace N` is a no-op / fails silently — acceptable.

## Testing

Manual smoke test, exercised after install:

1. `echo '{"session_id":"t1","cwd":"/Users/me/p1"}' | sketchybar-set-state running`
   → JSON file present, parent count `1`, label color yellow, popup
   shows `● <focused-ws> p1 running`.
2. `echo '{"session_id":"t2","cwd":"/Users/me/p2"}' | sketchybar-set-state needs-attention`
   → count `2`, parent color red, two rows; `needs-attention` sorts
   first.
3. `echo '{"session_id":"t1"}' | sketchybar-set-state clear`
   → `t1` row gone, count `1`, parent yellow.
4. Click parent pill → popup toggles. Click a row → AeroSpace switches
   to that workspace and popup closes.
5. `echo '{"session_id":"t2"}' | sketchybar-set-state clear` → count
   `0`, parent dim grey, popup empty.

No automated tests; sketchybar interactions are hard to drive headlessly
and the surface area is shell + state files.

## Out of scope

- Stale-session reaper (TTL or PID-based).
- Surfacing transcript paths or last-prompt previews.
- Disambiguating the workspace dot when multiple sessions share a
  workspace.
- Claude session metadata beyond `session_id`, `workspace`, `state`,
  `cwd`, `updated_at`.

## File map (new + changed)

```
claude-status/
├── .claude/plugins/local/claude-status/
│   └── bin/
│       ├── sketchybar-set-state          (modified)
│       └── claude-render-sessions        (new)
├── .config/sketchybar/plugins.d/
│   └── claude.sh                         (modified)
└── .local/bin/
    ├── sketchybar-set-state              (existing symlink)
    └── claude-render-sessions            (new symlink → ../../.claude/plugins/local/claude-status/bin/claude-render-sessions)
```

`install.sh` does not need changes — it already runs `stow` and reloads
sketchybar; new files come along automatically.
