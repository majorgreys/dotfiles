#!/usr/bin/env bash
# Register the stowed plugin path as a Claude Code marketplace and install
# the bundled plugin. Idempotent — safe to re-run.
#
# Stow handles the symlinks (sketchybar drop-in, .local/bin alias, and the
# plugin tree at ~/.claude/plugins/local/claude-status). This script handles
# the Claude Code side: registering the marketplace at that stowed path and
# enabling the plugin so its hooks load on every session start.

set -euo pipefail

PLUGIN_DIR="$HOME/.claude/plugins/local/claude-status"
MARKETPLACE_NAME="claude-status"
PLUGIN_NAME="claude-status"

if ! command -v claude >/dev/null 2>&1; then
  echo "error: claude CLI not found in PATH" >&2
  exit 1
fi

if [ ! -d "$PLUGIN_DIR" ]; then
  echo "error: $PLUGIN_DIR does not exist; run 'stow claude-status' first" >&2
  exit 1
fi

if claude plugin marketplace list 2>&1 | grep -q "$MARKETPLACE_NAME$"; then
  echo "marketplace '$MARKETPLACE_NAME' already registered; updating from source"
  claude plugin marketplace update "$MARKETPLACE_NAME" >/dev/null
else
  echo "registering marketplace at $PLUGIN_DIR"
  claude plugin marketplace add "$PLUGIN_DIR"
fi

if claude plugin list 2>&1 | grep -q "${PLUGIN_NAME}@${MARKETPLACE_NAME}"; then
  echo "plugin '${PLUGIN_NAME}@${MARKETPLACE_NAME}' already installed"
else
  echo "installing ${PLUGIN_NAME}@${MARKETPLACE_NAME}"
  claude plugin install "${PLUGIN_NAME}@${MARKETPLACE_NAME}"
fi

if pgrep -x sketchybar >/dev/null 2>&1; then
  sketchybar --reload >/dev/null 2>&1 || true
  echo "sketchybar reloaded"
fi

echo "done. open a new claude session to pick up the plugin's hooks."
