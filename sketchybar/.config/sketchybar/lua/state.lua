-- state.lua — shared mutable state for agent_status.
--
-- Tables are intentionally exposed for direct mutation. Helper functions
-- live in the modules that own the data flow.

local M = {}

-- session_id (string) -> {
--   workspace = "<n>",
--   state     = "submitted" | "running" | "tooling" | "needs-attention" | "idle" | "error" | "stale",
--   cwd       = "<path>",
--   updated_at = <epoch seconds>,
-- }
M.sessions = {}

-- workspace string -> aggregate state (most-urgent state of any session
-- pinned to that workspace). Kept by agent_status for pruning/accounting.
M.workspace_state = {}

-- urgency rank — higher = more urgent. Used when collapsing multiple
-- sessions in the same workspace down to a single dot color.
M.urgency = {
  ["error"]           = 6,
  ["needs-attention"] = 5,
  ["tooling"]         = 4,
  ["running"]         = 3,
  ["submitted"]       = 2,
  ["idle"]            = 1,
  ["stale"]           = 0,
}

return M
