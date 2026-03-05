## What (Structure)

macOS dotfiles managed with GNU Stow. Each subdirectory is a stow package that symlinks to `$HOME`.

Primary configs: Fish shell, Ghostty terminal, Doom Emacs, Neovim, Helix, Colima/Docker.

## Why (Purpose)

Personal development environment setup. Configs are minimal, version-controlled, and portable across machines.

## How (Workflow)

**Making changes:**
- Edit configs in their package directories (e.g., `fish/.config/fish/`)
- Test changes in a live shell/editor before committing
- Use `stow -R <package>` to re-stow after modifications
- Run `./setup.sh` to verify full installation works

**Stow structure notes:**
- `~/.config/fish` is a directory symlink to the stow source — edits to live path modify the repo directly
- `fish/.config/fish/conf.d/` is gitignored; tracked files there need `git add -f`
- LaunchAgents: place in `<package>/Library/LaunchAgents/` (e.g., `thbemacs/Library/LaunchAgents/`)

**Doom/thbemacs convergence:**
- thbemacs = lightweight org/notes driver (vanilla Emacs, single init.el)
- Doom = dev superset (multi-language LSP, tree-sitter, org-babel diagrams, confluence, org-db-v3)
- Both share a common core that must stay in sync:
  - TODO keywords: `TODO` / `DONE` (simple, tasks tracked in beads)
  - Logging: `org-log-done 'time`, `org-log-into-drawer t`
  - Vulpea: primary note interface, same config (describe-fn, template, journal)
  - Agent-shell: same viewport evil bindings, table renderer, attention, manager
  - Org-roam: stays as db layer for vulpea, same capture templates and dailies
  - Org-attach: same timestamp-based config (`org-id-method 'ts`, cp method)
  - Org-modern + org-tidy: same visual settings
  - Modus-themes: `modus-operandi` default, same toggle function
  - Keybindings: compatible SPC-leader namespace (general.el)
- Doom-only features (not in thbemacs): lsp-mode, tree-sitter, multi-lang, org-babel (mermaid/D2/dot), org-confluence-publish, org-db-v3, persp-mode workspaces
- thbemacs-only features: eglot, tabspaces, org-appear, custom splash screen

**thbemacs (vanilla Emacs):**
- Daemon managed by LaunchAgent (`com.thbemacs.daemon`), socket name `thbemacs`
- Connect: `emacsclient -s thbemacs` or fish function `thbemacs`
- Uses dd-gopls (not vanilla gopls) with `GOPLS_DISABLE_MODULE_LOADS=1`
- LSP client: eglot (built-in), not lsp-mode
- native-comp requires `LIBRARY_PATH=/opt/homebrew/lib/gcc/current` in LaunchAgent env

**Local overrides (not synced):**
- Fish: `~/.config/fish/config.local.fish`
- Doom: `~/.config/doom/local.el` and `packages-local.el`
- Topgrade: `~/.config/topgrade.d/local.toml`

**Commits:**
- Format: `[dotfiles-xxx] type: description` (include beads issue ID when applicable)
- Use conventional commits format
- Only attribute human authors

**Issue tracking:**
- Beads (`bd`) for issue tracking, connected to dolt server at `100.98.116.79:30306`
- Run `bd prime` for workflow context
- `BEADS_DOLT_PASSWORD` set via `.envrc` (direnv)