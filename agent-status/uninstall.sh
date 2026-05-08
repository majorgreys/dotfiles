#!/usr/bin/env bash
# Uninstall the Claude Code plugin, remove the local marketplace, and
# drop the PATH alias.

set -euo pipefail

MARKETPLACE_NAME="agent-status-marketplace"
PLUGIN_NAME="agent-status"
USER_BIN_LINK="$HOME/.local/bin/sketchybar-set-state"

if command -v claude >/dev/null 2>&1; then
  if claude plugin list 2>/dev/null | grep -q "${PLUGIN_NAME}@${MARKETPLACE_NAME}"; then
    claude plugin uninstall "${PLUGIN_NAME}@${MARKETPLACE_NAME}" || true
    echo "uninstalled ${PLUGIN_NAME}@${MARKETPLACE_NAME}"
  fi
  if claude plugin marketplace list 2>/dev/null | grep -q "^[[:space:]]*❯[[:space:]]*${MARKETPLACE_NAME}$"; then
    claude plugin marketplace remove "$MARKETPLACE_NAME" || true
    echo "removed marketplace '$MARKETPLACE_NAME'"
  fi
fi

if [ -L "$USER_BIN_LINK" ]; then
  rm -f "$USER_BIN_LINK"
  echo "unlinked $USER_BIN_LINK"
fi

# Clear any leftover state files.
rm -rf "$HOME/.local/state/sketchybar/agents/"* "$HOME/.local/state/sketchybar/sessions/"* 2>/dev/null
sketchybar --trigger agent_state_change >/dev/null 2>&1 || true

echo "done."
