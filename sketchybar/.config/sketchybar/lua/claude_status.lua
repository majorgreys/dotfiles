-- claude_status.lua — claude_sessions parent pill + popup, push-driven.
--
-- The sketchybar-set-state shell helper is invoked from Claude Code
-- hooks (UserPromptSubmit, Stop, Notification, SessionEnd). It triggers
-- the `claude_agent_state_change` sketchybar event with these args:
--   action       = "write" | "clear"
--   session_id   = "<uuid>"
--   state        = "running" | "needs-attention" | "idle"   (write only)
--   cwd          = "<path>"                                 (write only)
--   workspace    = "<n>"                                    (always)
--
-- We update an in-memory sessions table, recompute the per-workspace
-- aggregate state (read by workspaces.lua), then rebuild the popup
-- children. No filesystem state for the dropdown. A small per-session
-- pin file (`sessions/<id>.ws`) lives on disk so the helper can
-- reuse a session's pinned workspace across hook invocations — that
-- lookup happens entirely in the helper, not here.

local sbar = require("sketchybar")
require("bar")
local state = require("state")

local STATE_COLORS = {
  error               = Colors.red,
  ["needs-attention"] = Colors.yellow,
  running             = Colors.green,
  idle                = Colors.dim_dark,
}

-- Pill is a bracket of three items: robot icon, status dot, count.
-- Splitting them lets each carry its own font/color and lets the dot
-- sit between robot and count. The popup attaches to the robot item.
--
-- Items added with position=right are prepended to the right group,
-- so the LAST item added appears leftmost. Add right-to-left order:
-- count first, then dot, then robot — yielding visual [robot dot count].
local count_item = sbar.add("item", "claude_sessions_count", {
  position = "right",
  icon = { string = "" },
  label = {
    string        = "0",
    font          = Fonts.regular,
    color         = Colors.fg,
    padding_left  = 2,
    padding_right = 10,
  },
})

local status_dot = sbar.add("item", "claude_sessions_status", {
  position = "right",
  icon = {
    string        = "\u{25CF}",
    font          = Fonts.regular,
    color         = Colors.dim_dark,
    padding_left  = 0,
    padding_right = 4,
  },
  label = { string = "" },
})

local parent = sbar.add("item", "claude_sessions", {
  position = "right",
  icon = {
    string        = "󰚩",
    font          = Fonts.icon,
    color         = Colors.fg,
    padding_left  = 10,
    padding_right = 6,
  },
  label = { string = "" },
  popup = {
    background = {
      color         = Colors.popup_bg,
      corner_radius = 8,
      border_width  = 1,
      border_color  = Colors.popup_border,
    },
    horizontal = false,
    align      = "right",
    y_offset   = 4,
  },
})

-- Bracket renders an underline beneath the three items, matching the
-- workspace-pill focus-bar style (2px tall, offset below the text).
-- Static magenta-cooler accent — distinct from the workspace blue
-- focus underline while staying in the Modus Vivendi Tinted palette.
sbar.add("bracket", "claude_sessions_pill", {
  parent.name, status_dot.name, count_item.name,
}, {
  background = {
    color         = Colors.magenta,
    corner_radius = 0,
    height        = 2,
    y_offset      = -14,
    border_width  = 0,
  },
})

-- Track current popup children so we can wipe them cleanly. Sketchybar
-- has no "remove all from popup" primitive; we keep the names ourselves.
local popup_children = {}

local function clear_popup()
  for _, name in ipairs(popup_children) do
    sbar.remove(name)
  end
  popup_children = {}
end

-- Hover-driven popup with a grace period. Closing happens via a
-- token-checked deferred callback so a cursor sweep from pill to row
-- doesn't slam the popup shut mid-traversal.
local CLOSE_DELAY_S = 0.8
local close_token = 0

local function open_popup()
  close_token = close_token + 1   -- cancel any pending close
  parent:set({ popup = { drawing = "on" } })
end

local function schedule_close()
  close_token = close_token + 1
  local my_token = close_token
  sbar.exec(string.format("sleep %.2f", CLOSE_DELAY_S), function()
    if close_token == my_token then
      parent:set({ popup = { drawing = "off" } })
    end
  end)
end

local function add_popup_item(name, props)
  table.insert(popup_children, name)
  local item = sbar.add("item", name, props)
  -- Subscribing every popup child means cursor hovering anywhere in
  -- the popup keeps it open; leaving the last child schedules a close.
  item:subscribe("mouse.entered", open_popup)
  item:subscribe("mouse.exited", schedule_close)
  return item
end

-- Recompute state.workspace_state from state.sessions.
local function recompute_workspace_state()
  local ws_state = {}
  for _, s in pairs(state.sessions) do
    local cur = ws_state[s.workspace]
    if not cur or (state.urgency[s.state] or 0) > (state.urgency[cur] or 0) then
      ws_state[s.workspace] = s.state
    end
  end
  state.workspace_state = ws_state
end

-- Sort sessions for popup render: state urgency desc → workspace asc → updated_at desc.
local function sorted_sessions()
  local list = {}
  for id, s in pairs(state.sessions) do
    table.insert(list, { id = id, session = s })
  end
  table.sort(list, function(a, b)
    local ua, ub = state.urgency[a.session.state] or 0, state.urgency[b.session.state] or 0
    if ua ~= ub then return ua > ub end
    local wa = tonumber(a.session.workspace) or 999
    local wb = tonumber(b.session.workspace) or 999
    if wa ~= wb then return wa < wb end
    return (a.session.updated_at or 0) > (b.session.updated_at or 0)
  end)
  return list
end

-- Pad a string with trailing spaces to a target length (mono-font column align).
local function pad(s, len)
  while #s < len do s = s .. " " end
  return s
end

-- Strip trailing slash and return basename of a unix path. Empty path → "?".
local function basename(path)
  if not path or path == "" then return "?" end
  path = path:gsub("/+$", "")
  local last = path:match("([^/]+)$")
  return last or "?"
end


-- Highest-urgency state across all sessions. Drives the status dot
-- color in the menu-bar pill. Returns the count for the label.
local function aggregate_state()
  local count = 0
  local top_state, top_rank = nil, -1
  for _, s in pairs(state.sessions) do
    count = count + 1
    local rank = state.urgency[s.state] or 0
    if rank > top_rank then
      top_rank, top_state = rank, s.state
    end
  end
  return count, top_state
end

local function rebuild_popup()
  clear_popup()

  add_popup_item("claude_sessions._header", {
    position = "popup." .. parent.name,
    icon = {
      string        = "AGENTS",
      font          = "PragmataPro Mono Liga:Bold:13.0",
      color         = Colors.dim,
      padding_left  = 12,
      padding_right = 12,
    },
    label = { string = "" },
  })

  -- Compute longest project basename so all rows align in the mono font.
  local max_proj = 0
  for _, item in pairs(state.sessions) do
    local b = basename(item.cwd)
    if #b > max_proj then max_proj = #b end
  end

  for _, entry in ipairs(sorted_sessions()) do
    local s = entry.session
    local proj  = pad(basename(s.cwd), max_proj)
    local label = "\u{E861}  " .. proj .. "    " .. tostring(s.workspace)
    local short = entry.id:sub(1, 8)
    local child = "claude_sessions." .. short
    add_popup_item(child, {
      position = "popup." .. parent.name,
      icon = {
        string        = "\u{25CF}",
        font          = Fonts.regular,
        color         = STATE_COLORS[s.state],
        padding_left  = 12,
        padding_right = 10,
      },
      label = {
        string        = label,
        font          = Fonts.popup,
        color         = Colors.fg,
        padding_left  = 18,
        padding_right = 12,
      },
      -- Prefer focusing the exact aerospace window for this session;
      -- fall back to a workspace switch if window-id wasn't captured
      -- (e.g., a session pinned by an older helper version).
      click_script = (s.window_id and s.window_id ~= ""
        and ("aerospace focus --window-id " .. s.window_id)
        or  ("aerospace workspace " .. s.workspace))
        .. " && sketchybar --set claude_sessions popup.drawing=off",
    })
  end
end

local function repaint_parent()
  local count, top_state = aggregate_state()
  count_item:set({
    label = { string = tostring(count) },
  })
  status_dot:set({
    icon = { color = STATE_COLORS[top_state] or Colors.dim_dark },
  })
end

-- Subscribe to the helper-fired event. env contains the kv args from
-- `sketchybar --trigger claude_agent_state_change action=write ...`.
parent:subscribe("claude_agent_state_change", function(env)
  local action     = env.action
  local session_id = env.session_id
  local workspace  = env.workspace
  local st         = env.state
  local cwd        = env.cwd

  if not session_id or session_id == "" then
    -- Manual nudge with no session_id — nothing to update in our table.
    return
  end

  if action == "clear" then
    state.sessions[session_id] = nil
  elseif action == "write" then
    if not workspace or workspace == "" or not st or st == "" then return end
    state.sessions[session_id] = {
      workspace  = workspace,
      window_id  = env.window_id or "",
      state      = st,
      cwd        = cwd or "",
      updated_at = os.time(),
    }
  else
    return
  end

  recompute_workspace_state()
  repaint_parent()
  rebuild_popup()

  -- Workspace pills depend on state.workspace_state — fire their event
  -- so they re-paint with the new aggregate.
  sbar.trigger("aerospace_workspace_change")
end)

for _, item in ipairs({ parent, status_dot, count_item }) do
  item:subscribe("mouse.entered", open_popup)
  item:subscribe("mouse.exited", schedule_close)
end

-- Remove any popup children left over from a prior run. Sketchybar's
-- hotload preserves items across reloads, but our `popup_children`
-- table starts empty, so old per-session items would otherwise become
-- orphans (rendered, but not tracked or rebuilt).
sbar.exec(
  [[sketchybar --query bar 2>/dev/null | jq -r '.items[]? | select(startswith("claude_sessions."))']],
  function(out)
    for name in out:gmatch("[^\n]+") do
      sbar.remove(name)
    end
  end
)

-- Initial paint so the pill shows "0" dim grey on startup.
repaint_parent()
