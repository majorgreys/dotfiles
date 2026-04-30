# claude-status sketchybar plugin
#
# Sourced by the base sketchybar config (see ~/.dotfiles/sketchybar/sketchybarrc)
# AFTER the right-side status items (clock, battery, volume, wifi) and the
# workspace pills have been added.
#
# Provides:
#   1. The claude_agent_state_change event.
#   2. Workspace-pill subscriptions so each pill repaints when state changes.
#   3. The `claude_sessions` parent item (status-group leftmost) plus its
#      popup; clicking the pill toggles the popup, and a renderer script
#      rebuilds children on every event tick.
#
# State contracts (read by both sides):
#   $XDG_STATE_HOME/sketchybar/agents/<workspace>      one of:
#     running | needs-attention | idle | (missing/empty for no dot)
#   $XDG_STATE_HOME/sketchybar/sessions/<id>.json      JSON record per
#     active session: {session_id, workspace, state, cwd, updated_at}.

sketchybar --add event claude_agent_state_change

for i in 1 2 3 4 5 6 7 8 9 10; do
  sketchybar --subscribe space.$i claude_agent_state_change
done

# Catppuccin colors (must match the workspace-pill plugin).
CLAUDE_COLOR_FG="0xffcdd6f4"
CLAUDE_COLOR_DIM="0xff7f849c"
CLAUDE_COLOR_PILL_BG="0xff1e1e2e"      # base
CLAUDE_COLOR_PILL_BORDER="0xff45475a"  # surface1
CLAUDE_COLOR_POPUP_BG="0xff181825"     # mantle
CLAUDE_COLOR_POPUP_BORDER="0xff313244" # surface0
CLAUDE_FONT_ICON="FiraCode Nerd Font Mono:Regular:23.0"
CLAUDE_FONT_REGULAR="PragmataPro Mono Liga:Regular:15.0"

# Parent item — added with `right` after the four status items, so it
# lands as the leftmost element of the right-side status group:
#   ... | claude_sessions | wifi | volume | battery | clock |
sketchybar --add item claude_sessions right \
           --set claude_sessions \
                 icon="󰚩" \
                 icon.font="$CLAUDE_FONT_ICON" \
                 icon.color="$CLAUDE_COLOR_FG" \
                 icon.padding_left=10 \
                 icon.padding_right=8 \
                 label="0" \
                 label.font="$CLAUDE_FONT_REGULAR" \
                 label.color="$CLAUDE_COLOR_DIM" \
                 label.padding_right=10 \
                 background.color="$CLAUDE_COLOR_PILL_BG" \
                 background.corner_radius=6 \
                 background.border_width=1 \
                 background.border_color="$CLAUDE_COLOR_PILL_BORDER" \
                 background.height=28 \
                 click_script="sketchybar --set claude_sessions popup.drawing=toggle" \
                 popup.background.color="$CLAUDE_COLOR_POPUP_BG" \
                 popup.background.corner_radius=8 \
                 popup.background.border_width=1 \
                 popup.background.border_color="$CLAUDE_COLOR_POPUP_BORDER" \
                 popup.horizontal=off \
                 popup.align=right \
                 popup.y_offset=4 \
                 script="$HOME/.local/bin/claude-render-sessions" \
           --subscribe claude_sessions claude_agent_state_change
