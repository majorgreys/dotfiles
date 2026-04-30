# claude-status

Claude Code plugin + sketchybar drop-in that lights up sketchybar workspace
pills based on Claude Code activity.

- Yellow dot — Claude is working (`UserPromptSubmit` fired).
- Red dot — Claude is waiting on you (`Stop` or `Notification` fired).
- No dot — no active session, or session ended.

Each Claude session is pinned to the AeroSpace workspace it was first invoked
in (so the dot stays put even if you switch focus mid-turn).

## Layout

The dotfiles dir is shaped to mirror exactly what stow lays out under `$HOME`:

```
~/.dotfiles/claude-status/                           # stow package source
├── .claude/plugins/local/claude-status/             # → ~/.claude/plugins/local/claude-status/
│   ├── .claude-plugin/marketplace.json              # local marketplace (source.source: "directory")
│   ├── hooks/hooks.json                             # the four Claude Code hook registrations
│   └── bin/sketchybar-set-state                     # canonical helper, on PATH when plugin enabled
├── .config/sketchybar/plugins.d/                    # → ~/.config/sketchybar/plugins.d/
│   └── claude.sh                                    # registers claude_agent_state_change event
├── .local/bin/                                      # → ~/.local/bin/
│   └── sketchybar-set-state                         # → ../../.claude/plugins/local/claude-status/bin/sketchybar-set-state (relative)
├── README.md
├── install.sh                                       # marketplace add + plugin install
└── uninstall.sh                                     # plugin uninstall + marketplace remove
```

Stow tree-folds: `~/.claude/plugins/local` becomes a symlink to the
package's `.claude/plugins/local`, so the plugin tree and marketplace
manifest are visible at the path Claude Code expects, without copying.
Claude Code then registers `~/.claude/plugins/local/claude-status` as a
`directory`-source marketplace.

## Install

```bash
cd ~/.dotfiles && stow claude-status
~/.dotfiles/claude-status/install.sh
```

The install script:

1. Verifies `~/.claude/plugins/local/claude-status` exists (the stowed plugin).
2. Registers it as a Claude Code marketplace (`directory` source).
3. Installs the bundled plugin (`claude-status@claude-status`).
4. Reloads sketchybar.

Idempotent — safe to re-run after edits. Hooks come from the plugin's
`hooks/hooks.json` automatically; no `~/.claude/settings.json` edits needed.

After editing the plugin source in dotfiles, refresh Claude Code's loaded
copy with:

```bash
claude plugin marketplace update claude-status
```

## Uninstall

```bash
~/.dotfiles/claude-status/uninstall.sh
cd ~/.dotfiles && stow -D claude-status
```

## State-file contract

The base `sketchybar` package's workspace plugin reads:

```
$XDG_STATE_HOME/sketchybar/agents/<workspace>   # one of: running | needs-attention | idle
```

Missing or empty file = no dot. After writing, fire:

```bash
sketchybar --trigger claude_agent_state_change
```

## Manually nudging state (without Claude)

```bash
echo running > ~/.local/state/sketchybar/agents/3
sketchybar --trigger claude_agent_state_change
```

Or via the helper (which also handles session-id pinning):

```bash
echo '{"session_id":"manual-test"}' | sketchybar-set-state running
echo '{"session_id":"manual-test"}' | sketchybar-set-state clear
```
