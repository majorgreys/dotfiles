#!/usr/bin/env bash
set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_header() {
    echo -e "\n${BLUE}===================================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}===================================================${NC}\n"
}

print_success() { echo -e "${GREEN}✓ $1${NC}"; }
print_warning() { echo -e "${YELLOW}⚠ $1${NC}"; }
print_error() { echo -e "${RED}✗ $1${NC}"; }

command_exists() { command -v "$1" >/dev/null 2>&1; }

patch_emacs_plus_tap_for_homebrew_5() {
    local brew_prefix tap_dir emacs_base

    brew_prefix="$(brew --prefix)"
    tap_dir="$brew_prefix/Library/Taps/d12frosted/homebrew-emacs-plus"
    emacs_base="$tap_dir/Library/EmacsBase.rb"

    [[ -f "$emacs_base" ]] || return 0

    if grep -qE ':revision =>|:branch =>|:using =>' "$emacs_base"; then
        print_warning "Patching emacs-plus tap for Homebrew 5 URL keyword syntax..."
        perl -0pi -e 's/url EMACS_GIT_URL, :revision => rev/url EMACS_GIT_URL, revision: rev/g; s/url EmacsBase::EMACS_GIT_URL, :revision => rev/url EmacsBase::EMACS_GIT_URL, revision: rev/g; s/url EmacsBase::EMACS_GIT_URL, :branch => branch/url EmacsBase::EMACS_GIT_URL, branch: branch/g; s/url EMACS_GIT_URL, :branch => branch/url EMACS_GIT_URL, branch: branch/g; s/url \(\@\@urlResolver\.patch_url name\), :using => CopyDownloadStrategy/url @@urlResolver.patch_url(name), using: CopyDownloadStrategy/g' "$emacs_base"
        print_success "emacs-plus tap patched"
    fi
}

backup_existing_fontawesome_files() {
    local font_dir backup_dir found_font font

    font_dir="$HOME/Library/Fonts"
    [[ -d "$font_dir" ]] || return 0

    if brew list --cask font-fontawesome &>/dev/null; then
        return 0
    fi

    found_font=0
    for font in "$font_dir"/Font\ Awesome*; do
        [[ -e "$font" ]] || continue
        found_font=1
        break
    done

    [[ "$found_font" -eq 1 ]] || return 0

    backup_dir="$font_dir/.dotfiles-setup-backups/font-fontawesome-$(date +%Y%m%d%H%M%S)"
    mkdir -p "$backup_dir"
    print_warning "Moving existing Font Awesome files aside so Homebrew can install font-fontawesome..."
    for font in "$font_dir"/Font\ Awesome*; do
        [[ -e "$font" ]] || continue
        mv "$font" "$backup_dir/"
    done
    print_success "Existing Font Awesome files backed up to $backup_dir"
}

print_header "majorgreys dev setup for macos"

# Check macOS
if [[ "$OSTYPE" != "darwin"* ]]; then
    print_error "This script is designed for macOS only."
    exit 1
fi

# ===================================
# Install Homebrew
# ===================================
print_header "Checking Homebrew"
if command_exists brew; then
    print_success "Homebrew already installed: $(brew --version | head -n1)"
else
    print_warning "Installing Homebrew..."
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    # Add to PATH for Apple Silicon
    if [[ $(uname -m) == "arm64" ]]; then
        eval "$(/opt/homebrew/bin/brew shellenv)"
    fi
    print_success "Homebrew installed"
fi

# ===================================
# Install Brewfile packages
# ===================================
print_header "Installing Brewfile packages"

# Detect dotfiles directory (script location)
DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
INSTALL_DOOM="${SETUP_INSTALL_DOOM:-0}"
if [[ "$INSTALL_DOOM" == "true" || "$INSTALL_DOOM" == "yes" ]]; then
    INSTALL_DOOM=1
fi
if [[ -f "$DOTFILES_DIR/Brewfile" ]]; then
    # emacs-plus@31's tap has shipped formula helper code that is unreadable on
    # newer Homebrew releases (`url` now accepts option keywords, not a second
    # positional hash). Pre-tap and patch the helper before `brew bundle` parses
    # the formula. Disable auto-update for this bundle run so Homebrew does not
    # immediately overwrite the local compatibility patch.
    if grep -q 'emacs-plus@31' "$DOTFILES_DIR/Brewfile"; then
        brew tap d12frosted/emacs-plus
        patch_emacs_plus_tap_for_homebrew_5
    fi
    if grep -q 'font-fontawesome' "$DOTFILES_DIR/Brewfile"; then
        backup_existing_fontawesome_files
    fi

    # Ensure libgccjit is working before brew bundle (required by emacs-plus native compilation)
    if brew list libgccjit &>/dev/null && ! brew test libgccjit &>/dev/null 2>&1; then
        print_warning "Reinstalling libgccjit (test failed)..."
        brew reinstall libgccjit
    fi
    print_warning "Running 'brew bundle install'..."
    cd "$DOTFILES_DIR" && HOMEBREW_NO_AUTO_UPDATE=1 brew bundle install
    print_success "Brewfile packages installed"
else
    print_error "Brewfile not found at $DOTFILES_DIR/Brewfile"
    exit 1
fi

# Fix jpeg dylib mismatch: emacs-plus may be linked against libjpeg.9.dylib
# but jpeg was upgraded to v10. Create a compatibility symlink.
JPEG_LIB_DIR="/opt/homebrew/opt/jpeg/lib"
if [[ -d "$JPEG_LIB_DIR" && ! -f "$JPEG_LIB_DIR/libjpeg.9.dylib" ]]; then
    CURRENT_JPEG=$(ls "$JPEG_LIB_DIR"/libjpeg.*.dylib 2>/dev/null | head -1)
    if [[ -n "$CURRENT_JPEG" ]]; then
        print_warning "Creating jpeg compatibility symlink (libjpeg.9.dylib -> $(basename "$CURRENT_JPEG"))..."
        ln -sf "$CURRENT_JPEG" "$JPEG_LIB_DIR/libjpeg.9.dylib"
        print_success "jpeg compatibility symlink created"
    fi
fi

# Link Emacs 31 to Applications. Replace stale real app bundles from older
# Homebrew upgrades with a symlink so /opt/homebrew/bin/emacs does not prefer
# an obsolete /Applications/Emacs.app wrapper.
EMACS_APP=""
if [[ -d "/opt/homebrew/opt/emacs-plus@31/Emacs.app" ]]; then
    EMACS_APP="/opt/homebrew/opt/emacs-plus@31/Emacs.app"
elif [[ -d "/usr/local/opt/emacs-plus@31/Emacs.app" ]]; then
    EMACS_APP="/usr/local/opt/emacs-plus@31/Emacs.app"
fi
if [[ -n "$EMACS_APP" ]]; then
    EMACS_APP_LINK="/Applications/Emacs.app"

    if [[ -d "$EMACS_APP_LINK" && ! -L "$EMACS_APP_LINK" ]]; then
        STALE_EMACS_APP="/Applications/Emacs.app.stale-$(date +%Y%m%d%H%M%S)"
        if mv "$EMACS_APP_LINK" "$STALE_EMACS_APP" 2>/dev/null; then
            print_success "Moved stale Emacs.app to $STALE_EMACS_APP"
        else
            print_warning "Cannot replace $EMACS_APP_LINK without elevated permissions; linking Emacs in ~/Applications instead"
            mkdir -p "$HOME/Applications"
            EMACS_APP_LINK="$HOME/Applications/Emacs.app"
        fi
    fi

    if ln -sfn "$EMACS_APP" "$EMACS_APP_LINK" 2>/dev/null; then
        print_success "Linked Emacs.app -> $EMACS_APP_LINK"
    else
        print_warning "Cannot write $EMACS_APP_LINK; linking Emacs in ~/Applications instead"
        mkdir -p "$HOME/Applications"
        ln -sfn "$EMACS_APP" "$HOME/Applications/Emacs.app"
        print_success "Linked Emacs.app -> $HOME/Applications/Emacs.app"
    fi
fi

# Setup Emacs LaunchAgent
EMACS_PLIST="$HOME/Library/LaunchAgents/homebrew.mxcl.emacs-plus@31.plist"
EMACS_BIN="${EMACS_APP:-/opt/homebrew/opt/emacs-plus@31/Emacs.app}/Contents/MacOS/Emacs"
mkdir -p "$HOME/Library/LaunchAgents"

if [[ ! -f "$EMACS_PLIST" ]] || ! /usr/libexec/PlistBuddy -c "Print :ProgramArguments" "$EMACS_PLIST" &>/dev/null; then
    print_warning "Creating Emacs LaunchAgent..."

    # Create plist with required keys
    /usr/libexec/PlistBuddy -c "Add :Label string org.gnu.emacs.daemon" "$EMACS_PLIST" 2>/dev/null || true
    /usr/libexec/PlistBuddy -c "Add :ProgramArguments array" "$EMACS_PLIST" 2>/dev/null || true
    /usr/libexec/PlistBuddy -c "Add :ProgramArguments:0 string $EMACS_BIN" "$EMACS_PLIST" 2>/dev/null || true
    /usr/libexec/PlistBuddy -c "Add :ProgramArguments:1 string --daemon" "$EMACS_PLIST" 2>/dev/null || true
    /usr/libexec/PlistBuddy -c "Add :RunAtLoad bool true" "$EMACS_PLIST" 2>/dev/null || true
    /usr/libexec/PlistBuddy -c "Add :StandardErrorPath string /tmp/emacs-daemon.err" "$EMACS_PLIST" 2>/dev/null || true
    /usr/libexec/PlistBuddy -c "Add :StandardOutPath string /tmp/emacs-daemon.log" "$EMACS_PLIST" 2>/dev/null || true

    print_success "Emacs LaunchAgent created"
fi

# Add Ghostty TERMINFO if Ghostty is installed
if [[ -d "/Applications/Ghostty.app" ]]; then
    print_warning "Configuring Ghostty TERMINFO..."
    /usr/libexec/PlistBuddy -c "Add :EnvironmentVariables dict" "$EMACS_PLIST" 2>/dev/null || true
    /usr/libexec/PlistBuddy -c "Add :EnvironmentVariables:TERMINFO string /Applications/Ghostty.app/Contents/Resources/terminfo" "$EMACS_PLIST" 2>/dev/null || \
    /usr/libexec/PlistBuddy -c "Set :EnvironmentVariables:TERMINFO /Applications/Ghostty.app/Contents/Resources/terminfo" "$EMACS_PLIST"
    print_success "Ghostty TERMINFO configured"
fi

# ===================================
# Install Rust
# ===================================
print_header "Installing Rust"
if command_exists rustc; then
    print_success "Rust already installed: $(rustc --version)"
else
    print_warning "Installing Rust via rustup..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    source "$HOME/.cargo/env"
    print_success "Rust installed"
fi

if command_exists rust-analyzer; then
    print_success "rust-analyzer already installed"
else
    print_warning "Installing rust-analyzer..."
    rustup component add rust-analyzer
    print_success "rust-analyzer installed"
fi

# ===================================
# Install Python tools via uv
# ===================================
print_header "Installing Python tools"
print_warning "Installing Python tools via uv..."
uv tool install pyright
uv tool install ruff
uv tool install black
uv tool install pyflakes
uv tool install isort
uv tool install pipenv
uv tool install pytest
uv tool install nose
print_success "Python tools installed"

# ===================================
# Install Go tools
# ===================================
print_header "Installing Go tools"

# Install gopls
if command_exists gopls; then
    print_success "gopls already installed"
else
    print_warning "Installing gopls..."
    go install golang.org/x/tools/gopls@latest
    print_success "gopls installed"
fi

# Install additional Go tools for Doom Emacs
print_warning "Installing Go development tools..."
go install github.com/fatih/gomodifytags@latest
go install github.com/cweill/gotests/gotests@latest
go install github.com/x-motemen/gore/cmd/gore@latest
print_success "Go development tools installed"

# ===================================
# Install Beads (git-backed issue tracker)
# ===================================
print_header "Installing Beads"
if command_exists bd; then
    print_success "Beads already installed: $(bd version 2>/dev/null || echo 'installed')"
else
    print_warning "Installing Beads..."
    curl -fsSL https://raw.githubusercontent.com/steveyegge/beads/main/scripts/install.sh | bash
    print_success "Beads installed"
fi

# ===================================
# Setup dotfiles with stow
# ===================================
print_header "Setting up dotfiles with stow"

# Stow configs for installed tools
configs=("fish" "ghostty" "tmux" "vim" "helix")
if [[ "$INSTALL_DOOM" == "1" ]]; then
    configs+=("doom")
fi
configs+=("thbemacs" "starship" "aerospace" "sketchybar" "org-autosync")
for config in "${configs[@]}"; do
    if [[ -d "$DOTFILES_DIR/$config" ]]; then
        print_warning "Stowing $config config..."
        if (cd "$DOTFILES_DIR" && stow "$config" 2>/dev/null); then
            print_success "$config config stowed"
        else
            print_warning "stow $config failed (may already be stowed)"
        fi
    fi
done

# Packages that aren't stowed but install themselves into Claude Code as
# a marketplace + plugin. Their install.sh does the registration.
for pkg in agent-status; do
    if [[ -x "$DOTFILES_DIR/$pkg/install.sh" ]]; then
        print_warning "Running $pkg install..."
        "$DOTFILES_DIR/$pkg/install.sh" && print_success "$pkg installed" || print_warning "$pkg install failed"
    fi
done

# ===================================
# Install SbarLua (Lua bindings for sketchybar)
# ===================================
# SbarLua has no homebrew formula. Build from source and install the Lua
# C module to ~/.local/share/sketchybar_lua/sketchybar.so. Idempotent —
# rebuilds even if installed (the script is fast and cheap to re-run).
SBARLUA_SO="$HOME/.local/share/sketchybar_lua/sketchybar.so"
if [[ ! -f "$SBARLUA_SO" ]]; then
    print_warning "Installing SbarLua..."
    SBARLUA_TMP="$(mktemp -d)"
    if git clone --depth 1 https://github.com/FelixKratz/SbarLua.git "$SBARLUA_TMP" \
        && (cd "$SBARLUA_TMP" && make install >/dev/null); then
        rm -rf "$SBARLUA_TMP"
        print_success "SbarLua installed at $SBARLUA_SO"
    else
        rm -rf "$SBARLUA_TMP"
        print_warning "SbarLua install failed — sketchybar will not start until this is resolved"
    fi
else
    print_success "SbarLua already installed at $SBARLUA_SO"
fi

# ===================================
# Build merged PragmataPro+Claude font for sketchybar popups
# ===================================
# scripts/build-claude-font.py copies the Anthropic Claude glyph from
# Font Awesome 7 Brands (U+E861) into PragmataPro Mono Liga, producing
# "PragmataPro Mono Liga Claude" used by claude_status popup labels.
# Skipped silently when sources are missing — PragmataPro is commercial,
# the user must drop the .otf into ~/Library/Fonts/ themselves. Font
# Awesome comes from the font-fontawesome cask in Brewfile.
CLAUDE_FONT="$HOME/Library/Fonts/PragmataProMonoLigaClaude-Regular.otf"
PRAG_OTF="$HOME/Library/Fonts/PragmataPro_Mono_R_liga_0903.otf"
FA_OTF="$HOME/Library/Fonts/Font Awesome 7 Brands-Regular-400.otf"
if [[ ! -f "$CLAUDE_FONT" ]]; then
    if [[ -f "$PRAG_OTF" && -f "$FA_OTF" ]]; then
        print_warning "Building PragmataPro Mono Liga Claude font..."
        if "$DOTFILES_DIR/sketchybar/.config/sketchybar/scripts/build-claude-font.py" >/dev/null; then
            print_success "Claude font built at $CLAUDE_FONT"
        else
            print_warning "Claude font build failed — sketchybar popup glyphs will fall back"
        fi
    else
        print_warning "Skipping Claude font build — missing $([[ ! -f "$PRAG_OTF" ]] && echo "PragmataPro_Mono_R_liga_0903.otf ")$([[ ! -f "$FA_OTF" ]] && echo "Font Awesome 7 Brands-Regular-400.otf")in ~/Library/Fonts/"
    fi
else
    print_success "Claude font already built at $CLAUDE_FONT"
fi

# Setup fisher and fish plugins
if command_exists fish; then
    print_warning "Setting up fisher and fish plugins..."

    # Install fisher if not present
    if ! fish -c 'type -q fisher' 2>/dev/null; then
        print_warning "Installing fisher..."
        fish -c 'curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher'
        print_success "Fisher installed"
    else
        print_success "Fisher already installed"
    fi

    # Install plugins from fish_plugins file
    if [[ -f "$HOME/.config/fish/fish_plugins" ]]; then
        print_warning "Installing fish plugins..."
        fish -c 'fisher update'
        print_success "Fish plugins installed"
    fi
fi

# ===================================
# Setup Doom Emacs
# ===================================
if [[ "$INSTALL_DOOM" == "1" ]]; then
    print_header "Setting up Doom Emacs"

    DOOM_DIR="$HOME/.config/emacs"
    DOOM_BIN="$DOOM_DIR/bin/doom"

    if [[ -d "$DOOM_DIR" ]]; then
        print_success "Doom Emacs already cloned"
    else
        print_warning "Cloning Doom Emacs..."
        git clone --depth 1 https://github.com/doomemacs/doomemacs "$DOOM_DIR"
        print_success "Doom Emacs cloned"
    fi

    # Add doom to PATH for current session
    export PATH="$HOME/.config/emacs/bin:$PATH"

    # Install/sync Doom
    if [[ -f "$HOME/.config/emacs/.local/straight/repos/straight.el/README.md" ]]; then
        print_success "Doom already installed"
        print_warning "Running 'doom sync'..."
        "$DOOM_BIN" sync --yes
    else
        print_warning "Running 'doom install'..."
        "$DOOM_BIN" install
        print_success "Doom installed"
    fi
else
    print_header "Skipping Doom Emacs"
    print_warning "Doom setup disabled; set SETUP_INSTALL_DOOM=1 to re-enable"
fi

# ===================================
# Load/Restart Emacs LaunchAgents
# ===================================
print_header "Loading Emacs LaunchAgents"

# Generate org-autosync LaunchAgent from template
AUTOSYNC_PLIST="$HOME/Library/LaunchAgents/com.thb.org-autosync.plist"
AUTOSYNC_TEMPLATE="$DOTFILES_DIR/templates/com.thb.org-autosync.plist.template"
if [[ -f "$AUTOSYNC_TEMPLATE" ]]; then
    print_warning "Generating org-autosync LaunchAgent..."
    mkdir -p "$HOME/Library/LaunchAgents"
    mkdir -p "$HOME/.local/share/org-autosync"
    sed "s|__HOME__|$HOME|g" "$AUTOSYNC_TEMPLATE" > "$AUTOSYNC_PLIST"
    print_success "org-autosync LaunchAgent generated"

    if launchctl list com.thb.org-autosync &>/dev/null; then
        print_warning "Reloading org-autosync..."
        launchctl unload "$AUTOSYNC_PLIST"
        launchctl load "$AUTOSYNC_PLIST"
        print_success "org-autosync reloaded"
    else
        print_warning "Starting org-autosync..."
        launchctl load "$AUTOSYNC_PLIST"
        print_success "org-autosync started"
    fi
fi

# Generate thbemacs LaunchAgent from template
THBEMACS_PLIST="$HOME/Library/LaunchAgents/com.thbemacs.daemon.plist"
THBEMACS_TEMPLATE="$DOTFILES_DIR/templates/com.thbemacs.daemon.plist.template"
if [[ -f "$THBEMACS_TEMPLATE" ]]; then
    print_warning "Generating thbemacs LaunchAgent..."
    mkdir -p "$HOME/Library/LaunchAgents"
    sed "s|__HOME__|$HOME|g" "$THBEMACS_TEMPLATE" > "$THBEMACS_PLIST"
    print_success "thbemacs LaunchAgent generated"

    if launchctl list com.thbemacs.daemon &>/dev/null; then
        print_warning "Reloading thbemacs daemon..."
        launchctl unload "$THBEMACS_PLIST"
        launchctl load "$THBEMACS_PLIST"
        print_success "thbemacs daemon reloaded"
    else
        print_warning "Starting thbemacs daemon..."
        launchctl load "$THBEMACS_PLIST"
        print_success "thbemacs daemon started"
    fi
fi

if [[ "$INSTALL_DOOM" == "1" ]]; then
    # Generate Doom Emacs LaunchAgent from template
    DOOM_PLIST="$HOME/Library/LaunchAgents/com.thb.doom-emacs.plist"
    DOOM_TEMPLATE="$DOTFILES_DIR/templates/com.thb.doom-emacs.plist.template"
    if [[ -f "$DOOM_TEMPLATE" ]]; then
        print_warning "Generating Doom Emacs LaunchAgent..."
        mkdir -p "$HOME/Library/LaunchAgents"
        mkdir -p "$HOME/.local/share/doom-emacs"
        sed "s|__HOME__|$HOME|g" "$DOOM_TEMPLATE" > "$DOOM_PLIST"
        print_success "Doom Emacs LaunchAgent generated"

        # Unload old homebrew plist if it exists
        OLD_EMACS_PLIST="$HOME/Library/LaunchAgents/homebrew.mxcl.emacs-plus@30.plist"
        if launchctl list org.gnu.emacs.daemon &>/dev/null; then
            print_warning "Unloading old Emacs daemon..."
            launchctl unload "$OLD_EMACS_PLIST" 2>/dev/null || true
        fi

        if launchctl list com.thb.doom-emacs &>/dev/null; then
            print_warning "Reloading Doom Emacs daemon..."
            launchctl unload "$DOOM_PLIST"
            launchctl load "$DOOM_PLIST"
            print_success "Doom Emacs daemon reloaded"
        else
            print_warning "Starting Doom Emacs daemon..."
            launchctl load "$DOOM_PLIST"
            print_success "Doom Emacs daemon started"
        fi
    fi

    # ===================================
    # Run doom doctor
    # ===================================
    print_header "Running Doom Doctor"
    if [[ -x "$DOOM_BIN" ]]; then
        print_warning "Checking Doom configuration..."
        if "$DOOM_BIN" doctor; then
            print_success "doom doctor completed!"
        else
            print_warning "doom doctor found issues - please review above"
        fi
    fi
fi

# ===================================
# Configure Docker Compose Plugin
# ===================================
print_header "Configuring Docker Compose Plugin"

# Create Docker CLI plugins directory
if [[ ! -d "$HOME/.docker/cli-plugins" ]]; then
    print_warning "Creating Docker CLI plugins directory..."
    mkdir -p "$HOME/.docker/cli-plugins"
    print_success "Created ~/.docker/cli-plugins"
fi

# Symlink docker-compose as a Docker plugin
DOCKER_COMPOSE_BIN="$(brew --prefix)/opt/docker-compose/bin/docker-compose"
DOCKER_COMPOSE_PLUGIN="$HOME/.docker/cli-plugins/docker-compose"

if [[ -f "$DOCKER_COMPOSE_BIN" ]]; then
    if [[ -L "$DOCKER_COMPOSE_PLUGIN" ]]; then
        print_success "Docker Compose plugin already configured"
    else
        print_warning "Symlinking docker-compose as Docker plugin..."
        ln -sfn "$DOCKER_COMPOSE_BIN" "$DOCKER_COMPOSE_PLUGIN"
        print_success "Docker Compose plugin configured"
    fi
else
    print_warning "docker-compose binary not found at $DOCKER_COMPOSE_BIN"
fi

# ===================================
# Start Colima
# ===================================
print_header "Starting Colima"
if colima status &>/dev/null; then
    print_success "Colima already running"
else
    print_warning "Starting Colima..."
    colima start
    print_success "Colima started"
fi

# ===================================
# Test Colima & Docker
# ===================================
print_header "Testing Colima & Docker"

if [[ -x "$DOTFILES_DIR/test-docker.sh" ]]; then
    "$DOTFILES_DIR/test-docker.sh"
else
    print_warning "test-docker.sh not found, running basic tests..."

    # Test Colima status
    print_warning "Checking Colima status..."
    if colima status &>/dev/null; then
        COLIMA_STATUS=$(colima status 2>&1 | head -n1)
        print_success "Colima is running: $COLIMA_STATUS"
    else
        print_error "Colima is not running"
    fi

    # Test Docker daemon
    print_warning "Testing Docker daemon..."
    if docker info &>/dev/null; then
        DOCKER_VERSION=$(docker version --format '{{.Server.Version}}' 2>/dev/null)
        print_success "Docker daemon is accessible (version: $DOCKER_VERSION)"
    else
        print_error "Docker daemon is not accessible"
    fi

    # Test Docker with a simple command
    print_warning "Testing Docker functionality..."
    if docker ps &>/dev/null; then
        CONTAINER_COUNT=$(docker ps -q | wc -l | tr -d ' ')
        print_success "Docker is working (running containers: $CONTAINER_COUNT)"
    else
        print_error "Docker ps command failed"
    fi
fi

# ===================================
# Summary
# ===================================
print_header "Setup Complete!"
echo -e "${GREEN}Your development environment is ready!${NC}\n"

echo "Installed components:"
if [[ "$INSTALL_DOOM" == "1" ]]; then
    echo "  ✓ Editors: Emacs, Doom, Neovim, Helix"
else
    echo "  ✓ Editors: Emacs, Neovim, Helix"
fi
echo "  ✓ Python $(python3 --version 2>/dev/null | cut -d' ' -f2 || echo 'installed') + uv, pyright, ruff, black"
echo "  ✓ Go $(go version 2>/dev/null | cut -d' ' -f3 || echo 'installed') + gopls"
echo "  ✓ Rust $(rustc --version 2>/dev/null | cut -d' ' -f2 || echo 'installed') + rust-analyzer"
echo "  ✓ Zig $(zig version 2>/dev/null || echo 'installed')"
echo "  ✓ Shells: Fish, tmux"
echo "  ✓ Colima & Docker (tested and working)"
echo "  ✓ CLI tools: bat, eza, htop, jq, yq, zoxide, ripgrep, fd, fzf"
echo "  ✓ Dev tools: git, gh, git-delta, stow, direnv"
echo "  ✓ Programming fonts"

echo -e "\n${YELLOW}Next steps:${NC}"
echo "  1. Restart your shell:"
echo "     ${BLUE}exec \$SHELL${NC}"
echo ""
echo "  2. Your Doom config is at: ${BLUE}$HOME/.config/doom${NC}"
echo ""
echo "  3. Review 'doom doctor' output above"
echo ""
echo "  4. After modifying config, run: ${BLUE}doom sync${NC}"
echo ""
echo "  5. Colima commands:"
echo "     ${BLUE}colima status${NC} / ${BLUE}colima stop${NC} / ${BLUE}colima start${NC}"

print_success "Happy coding!"
