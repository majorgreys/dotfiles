# agent-status

Claude Code plugin that pushes per-session agent state into a
SbarLua-driven sketchybar config.

- Workspace pill dot — colored by the most-urgent agent state of any
  session pinned to that workspace (green=running, yellow=needs-attention,
  dim=idle, hidden=no agent activity).
- `agent_sessions` dropdown — bordered pill on the right side with
  count + popup listing every active session, grouped by state. Click a
  row to jump to that AeroSpace workspace.

## Layout

```
~/.dotfiles/agent-status/                           # stow package source
├── .claude/plugins/local/agent-status/             # → ~/.claude/plugins/local/agent-status/
│   ├── .claude-plugin/                              # plugin + marketplace manifest
│   │   ├── marketplace.json                         # local marketplace (source.source: "directory")
│   │   └── plugin.json                              # hooks (UserPromptSubmit, Stop, Notification, SessionEnd)
│   └── bin/sketchybar-set-state                     # the canonical helper, on PATH when plugin enabled
├── .local/bin/                                      # → ~/.local/bin/
│   └── sketchybar-set-state                         # → ../../.claude/plugins/local/agent-status/bin/sketchybar-set-state
├── tests/                                           # smoke tests (NOT stowed)
│   ├── smoke.sh
│   └── test-helpers.sh
├── README.md
├── install.sh                                       # marketplace add + plugin install
└── uninstall.sh                                     # plugin uninstall + marketplace remove
```

The bar config lives in the sibling `sketchybar` stow package, written
in Lua against [SbarLua](https://github.com/FelixKratz/SbarLua):

```
~/.dotfiles/sketchybar/.config/sketchybar/
├── sketchybarrc                # `#!/usr/bin/env lua` entry point
└── lua/
    ├── bar.lua                 # bar shell + Catppuccin palette + fonts
    ├── state.lua               # shared mutable state (sessions table)
    ├── workspaces.lua          # AeroSpace pills 1..10
    ├── status/                 # right-side status items
    │   ├── clock.lua
    │   ├── battery.lua
    │   ├── volume.lua
    │   └── wifi.lua
    └── agent_status.lua       # agent_sessions parent + popup
```

## Install

```bash
cd ~/.dotfiles && stow agent-status
~/.dotfiles/agent-status/install.sh
```

Prereqs (handled by the dotfiles' `setup.sh`):

- `brew install lua`
- SbarLua compiled into `~/.local/share/sketchybar_lua/sketchybar.so`
  (built from source — `setup.sh` does this automatically).
- Sketchybar reloaded at the end so the new Lua entry runs.

`install.sh`:

1. Verifies `~/.claude/plugins/local/agent-status` exists (the stowed
   plugin tree).
2. Registers it as a Claude Code marketplace (`directory` source).
3. Installs the bundled plugin (`agent-status@agent-status`).
4. Reloads sketchybar.

Idempotent — safe to re-run after edits. Hooks come from the plugin's
`.claude-plugin/plugin.json` automatically; no `~/.claude/settings.json`
edits needed.

After editing the plugin source in dotfiles, refresh Claude Code's
loaded copy with:

```bash
claude plugin marketplace update agent-status
```

## Uninstall

```bash
~/.dotfiles/agent-status/uninstall.sh
cd ~/.dotfiles && stow -D agent-status
```

## Push protocol

The Claude hooks invoke `sketchybar-set-state <state>` with hook JSON
on stdin. The helper:

1. Reads `.session_id` and `.cwd` from the hook payload.
2. Resolves the session's pinned AeroSpace workspace — re-using
   `$XDG_STATE_HOME/sketchybar/sessions/<id>.ws` if present (one-line
   text file containing the workspace number), else asking
   `aerospace list-workspaces --focused` and pinning.
3. Triggers the sketchybar `agent_state_change` event with kv
   args:

   ```
   sketchybar --trigger agent_state_change \
     action=<write|clear> \
     session_id=<id> \
     workspace=<n> \
     state=<running|needs-attention|idle>   # write only
     cwd=<path>                              # write only
   ```

4. Logs one tab-separated line to `$STATE_ROOT/helper.log`:

   ```
   <ISO time>  <state-arg>  <action>  <sid8>  <ws>  <cwd>
   ```

   Where `action` ∈ `{write, clear, skip-no-ws}`.
   Tail with `tail -f ~/.local/state/sketchybar/helper.log`.

The Lua handler in `agent_status.lua` updates an in-memory sessions
table, recomputes the per-workspace aggregate, repaints the parent
pill's count + color, and rebuilds the popup. No on-disk state for the
dropdown survives a sketchybar reload — the next Claude hook event
re-populates it.

## Manually nudging state

```bash
echo '{"session_id":"manual-test","cwd":"/tmp"}' \
  | sketchybar-set-state running
echo '{"session_id":"manual-test"}' \
  | sketchybar-set-state clear
```

## Tests

```bash
~/.dotfiles/agent-status/tests/smoke.sh
```

Runs the bash helper through a tempdir-`$XDG_STATE_HOME` + mocked
`aerospace`/`sketchybar` and asserts pin-file shape, trigger args, log
lines, and skip-on-no-workspace fallback.
