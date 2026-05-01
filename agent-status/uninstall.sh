#!/usr/bin/env bash
# Uninstall the Claude Code plugin and remove the local marketplace.
# Symlinks created by stow are removed separately with `stow -D agent-status`.

set -euo pipefail

MARKETPLACE_NAME="agent-status"
PLUGIN_NAME="agent-status"

if command -v claude >/dev/null 2>&1; then
  if claude plugin list 2>/dev/null | grep -q "${PLUGIN_NAME}@${MARKETPLACE_NAME}"; then
    claude plugin uninstall "${PLUGIN_NAME}@${MARKETPLACE_NAME}" || true
    echo "uninstalled ${PLUGIN_NAME}@${MARKETPLACE_NAME}"
  fi
  if claude plugin marketplace list 2>/dev/null | grep -q "^$MARKETPLACE_NAME\b"; then
    claude plugin marketplace remove "$MARKETPLACE_NAME" || true
    echo "removed marketplace '$MARKETPLACE_NAME'"
  fi
fi

# Clear any leftover state files.
rm -rf "$HOME/.local/state/sketchybar/agents/"* "$HOME/.local/state/sketchybar/sessions/"* 2>/dev/null
sketchybar --trigger agent_state_change >/dev/null 2>&1 || true

echo "done."
