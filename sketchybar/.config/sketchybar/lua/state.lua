-- state.lua — shared mutable state between agent_status and workspaces.
--
-- Both modules `require("state")` and read/write its tables. The shared
-- module pattern decouples the writer (agent_status) from the reader
-- (workspaces) without imposing a load-order constraint on sketchybarrc.
--
-- Tables are intentionally exposed for direct mutation. Helper functions
-- live in the modules that own the data flow (agent_status writes,
-- workspaces reads).

local M = {}

-- session_id (string) -> {
--   workspace = "<n>",
--   state     = "running" | "needs-attention" | "idle",
--   cwd       = "<path>",
--   updated_at = <epoch seconds>,
-- }
M.sessions = {}

-- workspace string -> aggregate state (most-urgent state of any session
-- pinned to that workspace). Recomputed by agent_status whenever
-- sessions changes.
M.workspace_state = {}

-- urgency rank — higher = more urgent. Used when collapsing multiple
-- sessions in the same workspace down to a single dot color.
M.urgency = {
  ["error"]           = 4,
  ["needs-attention"] = 3,
  ["running"]         = 2,
  ["idle"]            = 1,
}

return M
