# sketchybar

Minimal i3/sway-style status bar for macOS, driven by AeroSpace.

- Left-side AeroSpace focus pill: active workspace number + focused window title.
- Right-side modules: low-battery indicator (below 25%) and clock — Nerd Font glyphs, no labels.
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

# Tell AeroSpace to fire `aerospace_workspace_change` and
# `aerospace_focus_change` (already handled in the aerospace package)

# Start the service
brew services start sketchybar
```

## Hide the macOS menu bar (optional)

System Settings → Control Center → "Automatically hide and show the menu bar
in the menu bar" → Always.

## AeroSpace event contract

The `aerospace` package pushes two Sketchybar events:

- `aerospace_workspace_change` with `FOCUSED_WORKSPACE=<workspace>`
- `aerospace_focus_change` whenever AeroSpace focus changes

`lua/workspaces.lua` uses those events to repaint the single left-side focus
pill by querying the focused AeroSpace window/workspace. It queries AeroSpace
only through async `sbar.exec`; do not use `io.popen` or `os.execute` in the
long-lived SbarLua process.
