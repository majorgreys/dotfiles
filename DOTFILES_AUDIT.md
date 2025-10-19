# Dotfiles Audit - macOS vs Linux Configs

## Currently Active (in setup-dev-mac.sh)

✅ **fish** - Fish shell configuration (actively stowed)
✅ **tmux** - Terminal multiplexer (actively stowed)
✅ **vim** - Vim editor (actively stowed)
✅ **helix** - Helix editor (actively stowed)
✅ **zsh** - Zsh shell (actively stowed)
✅ **doom** - Doom Emacs config (manually synced via ~/.config/doom)

## Potentially Useful for macOS

### Terminal Emulators
🟡 **alacritty** - Cross-platform GPU-accelerated terminal
  - Works on macOS
  - Alternative to Terminal.app, iTerm2, or Ghostty
  - Consider: Are you using this or Ghostty?

### Utilities
🟡 **dircolors** - LS_COLORS configuration
  - Works on macOS with coreutils
  - Useful for colorizing `ls` output
  - Recommend: Keep if you use `eza` or GNU `ls`

🟡 **topgrade** - System update automation
  - Works on macOS
  - Can update Homebrew, Rust, npm, etc.
  - Recommend: Keep - very useful for macOS

## Linux-Specific (X11/Wayland) - SAFE TO REMOVE

### Window Managers & Compositors
❌ **i3** - X11 window manager
❌ **sway** - Wayland compositor
❌ **xorg** - X11 configuration (.Xresources, .xinitrc)

### Status Bars
❌ **polybar** - X11 status bar
❌ **waybar** - Wayland status bar

### Application Launchers
❌ **rofi** - X11 application launcher
❌ **wofi** - Wayland application launcher

### Notifications & Locking
❌ **dunst** - X11 notification daemon
❌ **swaylock** - Wayland screen locker

### Terminal Emulators
❌ **foot** - Wayland-native terminal emulator

### Theming
❌ **gtk-3.0** - GTK3 theming (Linux-specific)
❌ **qt5ct** - Qt5 theming (Linux-specific)

## Email/PIM Suite - LINUX-SPECIFIC SETUP

❌ **mutt** - Terminal-based email client
❌ **notmuch** - Email indexing and search
❌ **imapnotify** - IMAP email notifications
❌ **khard** - Contact management (CardDAV)
❌ **vdirsyncer** - Calendar/contacts sync
❌ **org-capture** - Emacs org-mode desktop integration

**Note:** These are configured for Linux email workflow. macOS has native Mail.app, Calendar.app, and Contacts.app. If you're not using this email stack on macOS, these can be removed.

## Recommendations

### Keep (Actively Used)
- fish, tmux, vim, helix, zsh, doom

### Consider Keeping
- **alacritty** - Only if you use it instead of Ghostty
- **dircolors** - If you use colored ls/eza output
- **topgrade** - Highly recommended for macOS system updates

### Safe to Archive/Remove
All configs marked with ❌ above are Linux-specific and not applicable to macOS.

### Suggested Actions

1. **Create an archive branch:**
   ```bash
   git checkout -b archive/linux-configs
   git push origin archive/linux-configs
   ```

2. **Remove Linux-specific configs from main:**
   ```bash
   git checkout master
   git rm -r i3 sway waybar polybar rofi wofi xorg dunst swaylock foot gtk-3.0 qt5ct
   git rm -r mutt notmuch imapnotify khard vdirsyncer org-capture
   git commit -m "chore: remove Linux-specific configs (archived in archive/linux-configs branch)"
   ```

3. **Decide on alacritty:**
   - Remove if you're using Ghostty exclusively
   - Keep if you switch between terminal emulators

4. **Add topgrade to setup-dev-mac.sh** if you want automated system updates

## Summary

**Total configs:** 33
**Currently active:** 6 (18%)
**Potentially useful:** 3 (9%)
**Linux-specific:** 24 (73%)

**Recommendation:** Archive Linux configs to keep repo focused on macOS development environment.
