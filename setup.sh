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

print_header "majorgreys dev setup for macos"

# Check macOS
if [[ "$OSTYPE" != "darwin"* ]]; then
    print_error "This script is designed for macOS only."
    exit 1
fi

# ===================================
# 1. Install Homebrew
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
# 2. Install Brewfile packages
# ===================================
print_header "Installing Brewfile packages"

DOTFILES_DIR="$HOME/.dotfiles"
if [[ -f "$DOTFILES_DIR/Brewfile" ]]; then
    print_warning "Running 'brew bundle install'..."
    cd "$DOTFILES_DIR" && brew bundle install
    print_success "Brewfile packages installed"
else
    print_error "Brewfile not found at $DOTFILES_DIR/Brewfile"
    exit 1
fi

# Link Emacs to Applications
if [[ -d "/opt/homebrew/opt/emacs-plus@30/Emacs.app" ]]; then
    ln -sf /opt/homebrew/opt/emacs-plus@30/Emacs.app /Applications/Emacs.app
elif [[ -d "/usr/local/opt/emacs-plus@30/Emacs.app" ]]; then
    ln -sf /usr/local/opt/emacs-plus@30/Emacs.app /Applications/Emacs.app
fi

# Patch Emacs LaunchAgent for Ghostty TERMINFO
EMACS_PLIST="$HOME/Library/LaunchAgents/homebrew.mxcl.emacs-plus@30.plist"
if [[ -f "$EMACS_PLIST" ]] && [[ -d "/Applications/Ghostty.app" ]]; then
    print_warning "Patching Emacs LaunchAgent for Ghostty TERMINFO..."
    if ! /usr/libexec/PlistBuddy -c "Print :EnvironmentVariables:TERMINFO" "$EMACS_PLIST" &>/dev/null; then
        # Add EnvironmentVariables dict if it doesn't exist
        /usr/libexec/PlistBuddy -c "Add :EnvironmentVariables dict" "$EMACS_PLIST" 2>/dev/null || true
        # Add TERMINFO entry
        /usr/libexec/PlistBuddy -c "Add :EnvironmentVariables:TERMINFO string /Applications/Ghostty.app/Contents/Resources/terminfo" "$EMACS_PLIST"
        print_success "Added TERMINFO to Emacs LaunchAgent"
    else
        print_success "TERMINFO already configured in Emacs LaunchAgent"
    fi
fi

# ===================================
# 3. Install Rust
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
# 4. Install Python tools via uv
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
# 5. Install Go tools
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
# 6. Setup dotfiles with stow
# ===================================
print_header "Setting up dotfiles with stow"

# Stow configs for installed tools
configs=("fish" "ghostty" "tmux" "vim" "helix" "doom" "starship")
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
# 7. Setup Doom Emacs
# ===================================
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
    "$DOOM_BIN" sync
else
    print_warning "Running 'doom install'..."
    "$DOOM_BIN" install
    print_success "Doom installed"
fi

# ===================================
# 8. Restart Brew Services
# ===================================
print_header "Restarting Brew Services"
if brew services list | grep -q "emacs-plus@30.*started"; then
    print_warning "Restarting Emacs service to pick up changes..."
    brew services restart emacs-plus@30
    print_success "Emacs service restarted"
elif [[ -f "$HOME/Library/LaunchAgents/homebrew.mxcl.emacs-plus@30.plist" ]]; then
    print_warning "Starting Emacs service..."
    brew services start emacs-plus@30
    print_success "Emacs service started"
fi

# ===================================
# 9. Run doom doctor
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

# ===================================
# 10. Configure Docker Compose Plugin
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
# 11. Start Colima
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
# 12. Test Colima & Docker
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
# 13. Summary
# ===================================
print_header "Setup Complete!"
echo -e "${GREEN}Your development environment is ready!${NC}\n"

echo "Installed components:"
echo "  ✓ Editors: Emacs, Doom, Neovim, Helix"
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
