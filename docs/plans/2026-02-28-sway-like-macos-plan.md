# Sway-like macOS Desktop Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replicate the sway window manager experience on macOS using Aerospace + AltTab.

**Architecture:** Aerospace provides i3/sway-like tiling and virtual workspaces via a single TOML config. AltTab replaces macOS Cmd-Tab with a window-based switcher. Both install as Homebrew casks. Aerospace config is stowable; AltTab is GUI-configured.

**Tech Stack:** Aerospace (tiling WM), AltTab (window switcher), GNU Stow, Homebrew

---

### Task 1: Add Aerospace and AltTab to Brewfile

**Files:**
- Modify: `Brewfile:67` (before Fonts section)

**Step 1: Add cask entries**

Add these lines before the `# Fonts` section in `Brewfile`:

```
# Window Management
tap "nikitabobko/tap"
cask "nikitabobko/tap/aerospace"
cask "alt-tab"

```

**Step 2: Verify syntax**

Run: `brew bundle check`
Expected: Lists aerospace and alt-tab as missing (not installed yet)

**Step 3: Commit**

```bash
git add Brewfile
git commit -m "brew: add aerospace and alt-tab casks"
```

---

### Task 2: Create Aerospace stow package with config

**Files:**
- Create: `aerospace/.config/aerospace/aerospace.toml`

**Step 1: Create directory**

```bash
mkdir -p aerospace/.config/aerospace
```

**Step 2: Write aerospace.toml**

Create `aerospace/.config/aerospace/aerospace.toml` with:

```toml
config-version = 2

start-at-login = true
auto-reload-config = true

enable-normalization-flatten-containers = true
enable-normalization-opposite-orientation-for-nested-containers = true

default-root-container-layout = 'tiles'
default-root-container-orientation = 'auto'

# No gaps — matching sway's default_border pixel 3 / hide_edge_borders smart
[gaps]
    inner.horizontal = 0
    inner.vertical = 0
    outer.left = 0
    outer.bottom = 0
    outer.top = 0
    outer.right = 0

[key-mapping]
    preset = 'qwerty'

[mode.main.binding]
    # Layout
    alt-slash = 'layout tiles horizontal vertical'
    alt-comma = 'layout accordion horizontal vertical'
    alt-f = 'fullscreen'
    alt-shift-space = 'layout floating tiling'

    # Focus (hjkl)
    alt-h = 'focus left'
    alt-j = 'focus down'
    alt-k = 'focus up'
    alt-l = 'focus right'

    # Move window (Shift+hjkl)
    alt-shift-h = 'move left'
    alt-shift-j = 'move down'
    alt-shift-k = 'move up'
    alt-shift-l = 'move right'

    # Resize
    alt-minus = 'resize smart -50'
    alt-equal = 'resize smart +50'

    # Workspaces — switch (with auto-back-and-forth like sway)
    alt-1 = 'workspace 1 --auto-back-and-forth'
    alt-2 = 'workspace 2 --auto-back-and-forth'
    alt-3 = 'workspace 3 --auto-back-and-forth'
    alt-4 = 'workspace 4 --auto-back-and-forth'
    alt-5 = 'workspace 5 --auto-back-and-forth'
    alt-6 = 'workspace 6 --auto-back-and-forth'
    alt-7 = 'workspace 7 --auto-back-and-forth'
    alt-8 = 'workspace 8 --auto-back-and-forth'
    alt-9 = 'workspace 9 --auto-back-and-forth'
    alt-0 = 'workspace 10 --auto-back-and-forth'

    # Workspaces — move window to
    alt-shift-1 = 'move-node-to-workspace 1'
    alt-shift-2 = 'move-node-to-workspace 2'
    alt-shift-3 = 'move-node-to-workspace 3'
    alt-shift-4 = 'move-node-to-workspace 4'
    alt-shift-5 = 'move-node-to-workspace 5'
    alt-shift-6 = 'move-node-to-workspace 6'
    alt-shift-7 = 'move-node-to-workspace 7'
    alt-shift-8 = 'move-node-to-workspace 8'
    alt-shift-9 = 'move-node-to-workspace 9'
    alt-shift-0 = 'move-node-to-workspace 10'

    # Back-and-forth
    alt-tab = 'workspace-back-and-forth'
    alt-shift-tab = 'move-workspace-to-monitor --wrap-around next'

    # Service mode
    alt-shift-semicolon = 'mode service'

[mode.service.binding]
    esc = ['reload-config', 'mode main']
    r = ['flatten-workspace-tree', 'mode main']
    f = ['layout floating tiling', 'mode main']
    backspace = ['close-all-windows-but-current', 'mode main']
    alt-shift-h = ['join-with left', 'mode main']
    alt-shift-j = ['join-with down', 'mode main']
    alt-shift-k = ['join-with up', 'mode main']
    alt-shift-l = ['join-with right', 'mode main']
```

**Step 3: Commit**

```bash
git add aerospace/
git commit -m "aerospace: add sway-like tiling config"
```

---

### Task 3: Add Aerospace to stow list in setup.sh

**Files:**
- Modify: `setup.sh:174`

**Step 1: Add aerospace to the configs array**

Change:
```bash
configs=("fish" "ghostty" "tmux" "vim" "helix" "doom" "thbemacs" "starship")
```
To:
```bash
configs=("fish" "ghostty" "tmux" "vim" "helix" "doom" "thbemacs" "starship" "aerospace")
```

**Step 2: Commit**

```bash
git add setup.sh
git commit -m "setup: add aerospace to stow list"
```

---

### Task 4: Install and stow

**Step 1: Install casks**

```bash
brew bundle install
```

**Step 2: Stow aerospace config**

```bash
cd ~/.dotfiles && stow aerospace
```

**Step 3: Verify config symlinked**

```bash
ls -la ~/.config/aerospace/aerospace.toml
```

Expected: symlink to `~/.dotfiles/aerospace/.config/aerospace/aerospace.toml`

**Step 4: Disable macOS auto-rearrange Spaces**

```bash
defaults write com.apple.dock mru-spaces -bool false
killall Dock
```

**Step 5: Launch Aerospace**

Open Aerospace from Applications or Spotlight. Grant Accessibility permissions when prompted.

**Step 6: Launch AltTab**

Open AltTab from Applications or Spotlight. Grant Accessibility permissions when prompted.

**Step 7: Verify keybindings work**

- Alt+1 through Alt+9, Alt+0 — switch workspaces
- Alt+h/j/k/l — focus direction
- Alt+Tab — back-and-forth

**Step 8: Commit setup.sh change if not already committed**

Already committed in Task 3.

---

### Task 5: Final commit and push

**Step 1: Verify all changes**

```bash
git status
git log --oneline -5
```

**Step 2: Push**

```bash
git push
```
