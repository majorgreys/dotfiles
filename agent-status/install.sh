#!/usr/bin/env bash
# Register this directory as a Claude Code marketplace and install the
# bundled agent-status plugin. Idempotent — safe to re-run.
#
# This package is NOT stowed. The marketplace lives at the dotfiles path
# directly, so Claude Code resolves ${CLAUDE_PLUGIN_ROOT} to the in-repo
# plugin tree. Edits to plugin source are picked up by
# `claude plugin marketplace update agent-status-marketplace`.
#
# A single PATH alias for manual nudging is created at
# ~/.local/bin/sketchybar-set-state. The Claude hooks themselves do not
# need it (they invoke ${CLAUDE_PLUGIN_ROOT}/bin/...).

set -euo pipefail

PACKAGE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
MARKETPLACE_NAME="agent-status-marketplace"
PLUGIN_NAME="agent-status"
PLUGIN_BIN="$PACKAGE_DIR/plugins/agent-status/bin/sketchybar-set-state"
USER_BIN_LINK="$HOME/.local/bin/sketchybar-set-state"

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

if pgrep -x sketchybar >/dev/null 2>&1; then
  sketchybar --reload >/dev/null 2>&1 || true
  echo "sketchybar reloaded"
fi

echo "done. open a new claude session to pick up the plugin's hooks."
