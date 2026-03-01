# Sway-like macOS Desktop

## Goal

Replicate the sway window manager experience on macOS using Aerospace (tiling WM) and AltTab (window switcher).

## Previous Setup (sway)

- Super+1-9,0 for workspace switching (10 workspaces)
- Super+Shift+1-9,0 to move windows between workspaces
- Super+h/j/k/l for directional focus
- Super+Shift+h/j/k/l to move windows directionally
- workspace_auto_back_and_forth enabled
- Resize mode via Super+r
- Super+f for fullscreen, Super+Shift+Space for float toggle

## Tools

- **Aerospace** — i3/sway-like tiling WM for macOS, TOML config, no SIP changes needed
- **AltTab** — window-based Alt-Tab switcher (replaces macOS Cmd-Tab)

## Keybinding Mapping

Alt is used instead of Super/Cmd because macOS reserves Cmd for system shortcuts.

| Sway binding | Aerospace binding | Action |
|---|---|---|
| Super+1-9,0 | Alt+1-9,0 | Switch workspace (with auto-back-and-forth) |
| Super+Shift+1-9,0 | Alt+Shift+1-9,0 | Move window to workspace |
| Super+h/j/k/l | Alt+h/j/k/l | Focus direction |
| Super+Shift+h/j/k/l | Alt+Shift+h/j/k/l | Move window direction |
| Super+f | Alt+f | Fullscreen |
| Super+Shift+Space | Alt+Shift+Space | Toggle floating |
| Super+r | Alt+r | Enter resize mode |
| Super+b / Super+v | Alt+slash | Toggle h/v split |

## Config Location

- `aerospace/.config/aerospace/aerospace.toml` (stow package)
- AltTab: configured via GUI preferences (no config file to stow)

## macOS Settings

- Disable "Automatically rearrange Spaces based on most recent use"

## Dotfiles Changes

1. New stow package: `aerospace/`
2. Add `aerospace` and `alt-tab` casks to Brewfile
3. Add `aerospace` to stow list in setup.sh

## Caveat

AltTab filters by macOS Spaces, not Aerospace workspaces. It serves as a visual window picker across all workspaces.
