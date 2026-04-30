#!/usr/bin/env bash
# Renders one workspace pill: number + optional agent-state dot.
#
# Focus state comes from FOCUSED_WORKSPACE (set by AeroSpace's
# exec-on-workspace-change) or by querying aerospace on initial render.
#
# Agent state comes from a per-workspace file written by the cmux agent daemon:
#   $XDG_STATE_HOME/cmux/agents/<workspace>   (default ~/.local/state/cmux/agents/<n>)
# Contents (plain text, single token):
#   running          -> yellow dot (Claude is working)
#   needs-attention  -> red dot    (Claude is waiting on the user)
#   idle             -> gray dot   (session attached, no agent activity)
#   (missing/empty)  -> no dot
#
# To nudge the bar after writing state, the daemon should:
#   sketchybar --trigger cmux_agent_state_change

SPACE="$1"
CURRENT="${FOCUSED_WORKSPACE:-$(aerospace list-workspaces --focused 2>/dev/null)}"

# i3-bar style: focused workspace gets a thin accent-colored underline at the
# bottom (rendered via the item background — which is now a 2px tall, no-radius
# strip pinned to the bottom by background.y_offset in sketchybarrc).
if [ "$CURRENT" = "$SPACE" ]; then
  BG_COLOR="0xff89b4fa"
else
  BG_COLOR="0x00000000"
fi
FG_COLOR="0xffcdd6f4"

STATE_FILE="${XDG_STATE_HOME:-$HOME/.local/state}/sketchybar/agents/$SPACE"
if [ -s "$STATE_FILE" ]; then
  STATE=$(< "$STATE_FILE")
else
  STATE=""
fi

case "$STATE" in
  running)
    LABEL="●"
    LABEL_COLOR="0xffa6e3a1"
    LABEL_DRAW="on"
    ;;
  needs-attention)
    LABEL="●"
    LABEL_COLOR="0xfff9e2af"
    LABEL_DRAW="on"
    ;;
  idle)
    LABEL="●"
    LABEL_COLOR="0xff6c7086"
    LABEL_DRAW="on"
    ;;
  *)
    LABEL=""
    LABEL_COLOR="0x00000000"
    LABEL_DRAW="off"
    ;;
esac

sketchybar --set "$NAME" \
           background.color="$BG_COLOR" \
           icon.color="$FG_COLOR" \
           label="$LABEL" \
           label.color="$LABEL_COLOR" \
           label.drawing="$LABEL_DRAW"
