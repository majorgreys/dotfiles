# claude-status sketchybar plugin
#
# Sourced by the base sketchybar config (see ~/.dotfiles/sketchybar/sketchybarrc)
# after workspace items have been added. Registers the custom event that the
# claude-set-state helper triggers, and subscribes each workspace pill so its
# script re-runs and re-reads its agent-state file.
#
# Contract (read by base sketchybar/plugins/aerospace.sh):
#   $XDG_STATE_HOME/sketchybar/agents/<workspace>   one of:
#     running | needs-attention | idle | (missing/empty for no dot)

sketchybar --add event claude_agent_state_change

for i in 1 2 3 4 5 6 7 8 9 10; do
  sketchybar --subscribe space.$i claude_agent_state_change
done
