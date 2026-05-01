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

local STATE_WORDS = {
  running         = "working",
  ["needs-attention"] = "waiting",
  idle            = "idle",
}

local STATE_COLORS = {
  running         = Colors.green,
  ["needs-attention"] = Colors.yellow,
  idle            = Colors.dim_dark,
}

-- Section grouping order: working > waiting > idle, mirrors urgency.
local SECTION_ORDER = { "running", "needs-attention", "idle" }

-- Parent pill — leftmost item in the right-side status group.
local parent = sbar.add("item", "claude_sessions", {
  position = "right",
  icon = {
    string        = "󰚩",
    font          = Fonts.icon,
    color         = Colors.fg,
    padding_left  = 10,
    padding_right = 8,
  },
  label = {
    string        = "0",
    font          = Fonts.regular,
    color         = Colors.dim,
    padding_right = 10,
  },
  background = {
    color         = Colors.pill_bg,
    corner_radius = 6,
    border_width  = 1,
    border_color  = Colors.pill_border,
    height        = 28,
  },
  click_script = "sketchybar --set claude_sessions popup.drawing=toggle",
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

-- Track current popup children so we can wipe them cleanly. Sketchybar
-- has no "remove all from popup" primitive; we keep the names ourselves.
local popup_children = {}

local function clear_popup()
  for _, name in ipairs(popup_children) do
    sbar.remove(name)
  end
  popup_children = {}
end

local function add_popup_item(name, props)
  table.insert(popup_children, name)
  return sbar.add("item", name, props)
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

-- Aggregate state of all sessions → parent label color.
local function parent_color()
  local count = 0
  local has_needs, has_run = false, false
  for _, s in pairs(state.sessions) do
    count = count + 1
    if s.state == "needs-attention" then has_needs = true end
    if s.state == "running"         then has_run   = true end
  end
  if count == 0 then return count, Colors.dim end
  if has_needs then return count, Colors.yellow end
  if has_run   then return count, Colors.green end
  return count, Colors.fg
end

-- Group sessions by state for section rendering.
local function group_by_state(list)
  local groups = {}
  for _, st in ipairs(SECTION_ORDER) do groups[st] = {} end
  for _, item in ipairs(list) do
    local st = item.session.state
    if groups[st] then table.insert(groups[st], item) end
  end
  return groups
end

local function rebuild_popup()
  clear_popup()

  local n_run, n_needs, n_idle = 0, 0, 0
  for _, s in pairs(state.sessions) do
    if     s.state == "running"         then n_run   = n_run   + 1
    elseif s.state == "needs-attention" then n_needs = n_needs + 1
    elseif s.state == "idle"            then n_idle  = n_idle  + 1 end
  end

  add_popup_item("claude_sessions._header", {
    position = "popup." .. parent.name,
    icon = {
      string        = "AGENTS",
      font          = "PragmataPro Mono Liga:Bold:13.0",
      color         = Colors.dim,
      padding_left  = 12,
      padding_right = 18,
    },
    label = {
      string        = string.format("%d working · %d waiting · %d idle", n_run, n_needs, n_idle),
      color         = Colors.fg,
      padding_right = 12,
    },
  })

  -- Compute longest project basename so all rows align in the mono font.
  local max_proj = 0
  for _, item in pairs(state.sessions) do
    local b = basename(item.cwd)
    if #b > max_proj then max_proj = #b end
  end

  local list = sorted_sessions()
  local groups = group_by_state(list)
  local section_idx = 0

  for _, st in ipairs(SECTION_ORDER) do
    local group = groups[st]
    if #group > 0 then
      section_idx = section_idx + 1
      local section_name = "claude_sessions._sec_" .. section_idx
      add_popup_item(section_name, {
        position = "popup." .. parent.name,
        icon  = { string = "" },
        label = {
          string        = STATE_WORDS[st]:upper(),
          font          = Fonts.small,
          color         = Colors.dim_dark,
          padding_left  = 12,
          padding_right = 12,
        },
      })

      for _, entry in ipairs(group) do
        local s = entry.session
        local proj  = pad(basename(s.cwd), max_proj)
        local word  = pad(STATE_WORDS[st], 7)
        local short = entry.id:sub(1, 8)
        local child = "claude_sessions." .. short
        add_popup_item(child, {
          position = "popup." .. parent.name,
          icon = {
            string        = "●",
            color         = STATE_COLORS[st],
            padding_left  = 12,
            padding_right = 8,
          },
          label = {
            string        = string.format("%s  %s  %s", proj, word, s.workspace),
            color         = Colors.fg,
            padding_right = 12,
          },
          click_script = "aerospace workspace " .. s.workspace
                       .. " && sketchybar --set claude_sessions popup.drawing=off",
        })
      end
    end
  end
end

local function repaint_parent()
  local count, color = parent_color()
  parent:set({
    label = { string = tostring(count), color = color },
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

-- Initial paint so the pill shows "0" dim grey on startup.
repaint_parent()
