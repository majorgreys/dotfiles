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

**Local overrides (not synced):**
- Fish: `~/.config/fish/config.local.fish`
- Doom: `~/.config/doom/local.el` and `packages-local.el`
- Topgrade: `~/.config/topgrade.d/local.toml`

**Commits:**
- Use conventional commits format
- Only attribute human authors