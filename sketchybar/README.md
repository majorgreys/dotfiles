# sketchybar

Minimal i3/sway-style status bar for macOS, driven by AeroSpace.

- Workspace pills 1–10, focused pill carries an accent-colored underline.
- Right-side modules: wifi, volume, battery, clock — Nerd Font glyphs, no labels.
- Plain black background, Pragmata Mono Liga.
- Drop-in plugin slot at `~/.config/sketchybar/plugins.d/*.sh` (sourced by
  `sketchybarrc` after the base setup). See the `agent-status` package for
  an example.

## Install

```bash
brew install sketchybar
brew tap homebrew/cask-fonts
brew install --cask font-fira-code-nerd-font

# Stow this package
cd ~/.dotfiles && stow sketchybar

# Tell AeroSpace to fire `aerospace_workspace_change`
# (already handled in the aerospace dotfiles package)

# Start the service
brew services start sketchybar
```

## Hide the macOS menu bar (optional)

System Settings → Control Center → "Automatically hide and show the menu bar
in the menu bar" → Always.

## State-file contract for plugins

The base `aerospace.sh` script reads optional per-workspace state files at:

    $XDG_STATE_HOME/sketchybar/agents/<workspace-number>

Contents are a single token: `running` | `needs-attention` | `idle`. Missing
or empty file = no dot. Anything that wants to drive the dot writes to that
location and triggers a sketchybar event the workspace items subscribe to.
