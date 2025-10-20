## About This Repository

This is a macOS-focused dotfiles repository for development environment setup.

**Primary tools:**
- Shell: Fish (with starship prompt)
- Terminal: Ghostty
- Editors: Doom Emacs, Neovim, Helix
- Container runtime: Colima + Docker

**Setup:**
- Run `./setup.sh` for automated installation
- Configs managed with GNU Stow
- Archived Linux-specific configs in `archive/linux-configs` branch

**Local Configuration:**
Private/company-specific configs should be stored in local files that are not synced:
- Fish: `~/.config/fish/config.local.fish` (sourced automatically if exists)
- Doom Emacs: `~/.config/doom/local.el` and `packages-local.el` (loaded automatically if exist)

## Ethical Guidelines

- Do not attribute commits to non-human authors

## Development Practices

- Use conventional commits
- Test scripts before committing
- Keep configs minimal and documented