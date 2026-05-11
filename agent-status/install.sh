#!/usr/bin/env bash
# Install the agent-status family:
#   1. Register this directory as a Claude Code marketplace and
#      install the bundled `agent-status` plugin (claude hooks).
#   2. Symlink the pi extension `sketchybar-agent-status.ts` into
#      ~/.pi/agent/extensions/ so pi sessions push state too.
#   3. Symlink the helper + zmx wrappers (claude-z, pi-z) into
#      ~/.local/bin so they're on PATH.
#
# Idempotent — safe to re-run after edits. The pi extension is loaded
# fresh on every pi invocation; the claude marketplace is refreshed
# explicitly via `claude plugin marketplace update`.
#
# This package is NOT stowed. The marketplace lives at the dotfiles
# path directly, so Claude Code resolves ${CLAUDE_PLUGIN_ROOT} to the
# in-repo plugin tree. Edits to claude plugin source are picked up by
# `claude plugin marketplace update agent-status-marketplace`.
#
# The standalone TUI (`agent-panel`) lives in its own repo at
# ~/dev/majorgreys/agent-panel and is installed independently.

set -euo pipefail

PACKAGE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_NAME="agent-status-marketplace"
PLUGIN_NAME="agent-status"
PLUGIN_BIN="$PACKAGE_DIR/plugins/agent-status/bin/sketchybar-set-state"
USER_BIN_LINK="$HOME/.local/bin/sketchybar-set-state"

PI_EXT_SRC="$PACKAGE_DIR/plugins/agent-status-pi/sketchybar-agent-status.ts"
PI_EXT_DST="$HOME/.pi/agent/extensions/sketchybar-agent-status.ts"

MIGRATE_SRC="$PACKAGE_DIR/plugins/agent-status/bin/migrate-pins"
MIGRATE_DST="$HOME/.local/bin/migrate-pins"

CLAUDE_Z_SRC="$PACKAGE_DIR/plugins/zmx-wrappers/bin/claude-z"
CLAUDE_Z_DST="$HOME/.local/bin/claude-z"
PI_Z_SRC="$PACKAGE_DIR/plugins/zmx-wrappers/bin/pi-z"
PI_Z_DST="$HOME/.local/bin/pi-z"

if ! command -v claude >/dev/null 2>&1; then
  echo "error: claude CLI not found in PATH" >&2
  exit 1
fi

if [ ! -f "$PACKAGE_DIR/.claude-plugin/marketplace.json" ]; then
  echo "error: $PACKAGE_DIR/.claude-plugin/marketplace.json missing" >&2
  exit 1
fi

if [ ! -x "$PLUGIN_BIN" ]; then
  echo "error: $PLUGIN_BIN not executable" >&2
  exit 1
fi

if claude plugin marketplace list 2>&1 | grep -q "^[[:space:]]*❯[[:space:]]*${MARKETPLACE_NAME}$"; then
  echo "marketplace '$MARKETPLACE_NAME' already registered; updating from source"
  claude plugin marketplace update "$MARKETPLACE_NAME" >/dev/null
else
  echo "registering marketplace at $PACKAGE_DIR"
  claude plugin marketplace add "$PACKAGE_DIR"
fi

if claude plugin list 2>&1 | grep -q "${PLUGIN_NAME}@${MARKETPLACE_NAME}"; then
  echo "plugin '${PLUGIN_NAME}@${MARKETPLACE_NAME}' already installed"
else
  echo "installing ${PLUGIN_NAME}@${MARKETPLACE_NAME}"
  claude plugin install "${PLUGIN_NAME}@${MARKETPLACE_NAME}"
fi

mkdir -p "$(dirname "$USER_BIN_LINK")"
ln -sfn "$PLUGIN_BIN" "$USER_BIN_LINK"
echo "linked $USER_BIN_LINK -> $PLUGIN_BIN"

# pi extension symlink (skip silently if pi isn't installed).
if [ -f "$PI_EXT_SRC" ]; then
  mkdir -p "$(dirname "$PI_EXT_DST")"
  # If a non-symlink file already exists at the target, back it up.
  if [ -e "$PI_EXT_DST" ] && [ ! -L "$PI_EXT_DST" ]; then
    BACKUP="${PI_EXT_DST}.bak.$(date +%Y%m%d-%H%M%S)"
    mv "$PI_EXT_DST" "$BACKUP"
    echo "backed up existing $PI_EXT_DST -> $BACKUP"
  fi
  ln -sfn "$PI_EXT_SRC" "$PI_EXT_DST"
  echo "linked $PI_EXT_DST -> $PI_EXT_SRC"
fi

# zmx wrappers (claude-z, pi-z).
for pair in "$CLAUDE_Z_SRC|$CLAUDE_Z_DST" "$PI_Z_SRC|$PI_Z_DST" "$MIGRATE_SRC|$MIGRATE_DST"; do
  src="${pair%%|*}"
  dst="${pair##*|}"
  if [ -x "$src" ]; then
    mkdir -p "$(dirname "$dst")"
    ln -sfn "$src" "$dst"
    echo "linked $dst -> $src"
  fi
done

if pgrep -x sketchybar >/dev/null 2>&1; then
  sketchybar --reload >/dev/null 2>&1 || true
  echo "sketchybar reloaded"
fi

cat <<EOF

done.

  - claude:  open a new claude session (or run \`claude plugin marketplace
             update $MARKETPLACE_NAME\`) to pick up hook changes.
  - pi:      next \`pi\` invocation reloads the extension automatically.
  - launch agents in zmx so the panel can attach:
               claude-z  [-n NAME]
               pi-z      [-n NAME]
  - install the standalone TUI separately:
               cd ~/dev/majorgreys/agent-panel && ./install.sh
EOF
