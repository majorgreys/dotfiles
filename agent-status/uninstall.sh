#!/usr/bin/env bash
# Uninstall the Claude Code plugin, remove the local marketplace, and
# drop the PATH alias.

set -euo pipefail

MARKETPLACE_NAME="agent-status-marketplace"
PLUGIN_NAME="agent-status"
USER_BIN_LINK="$HOME/.local/bin/sketchybar-set-state"
PI_EXT_DST="$HOME/.pi/agent/extensions/sketchybar-agent-status.ts"
CLAUDE_Z_DST="$HOME/.local/bin/claude-z"
PI_Z_DST="$HOME/.local/bin/pi-z"
MIGRATE_DST="$HOME/.local/bin/migrate-pins"

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

for link in "$USER_BIN_LINK" "$PI_EXT_DST" "$CLAUDE_Z_DST" "$PI_Z_DST" "$MIGRATE_DST"; do
  if [ -L "$link" ]; then
    rm -f "$link"
    echo "unlinked $link"
  fi
done

# Clear any leftover state files.
rm -rf "$HOME/.local/state/sketchybar/agents/"* "$HOME/.local/state/sketchybar/sessions/"* 2>/dev/null
sketchybar --trigger agent_state_change >/dev/null 2>&1 || true

echo "done."
