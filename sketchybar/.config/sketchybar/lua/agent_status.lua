-- agent_status.lua — agent_sessions parent pill + popup, push-driven.
--
-- The sketchybar-set-state shell helper is invoked from Claude Code
-- hooks (UserPromptSubmit, Stop, Notification, SessionEnd). It triggers
-- the `agent_state_change` sketchybar event with these args:
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
local count_item = sbar.add("item", "agent_sessions_count", {
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

local status_dot = sbar.add("item", "agent_sessions_status", {
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

local parent = sbar.add("item", "agent_sessions", {
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
sbar.add("bracket", "agent_sessions_pill", {
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

-- Fixed pixel width for every popup row so the popup doesn't jitter
-- as agents come and go. POPUP_WIDTH and TARGET_LABEL_CHARS are tuned
-- in lockstep so the right-justified workspace number sits flush
-- against the inside edge of the popup.
local POPUP_WIDTH = 260
local TARGET_LABEL_CHARS = 22

local function add_popup_item(name, props, opts)
  opts = opts or {}
  table.insert(popup_children, name)
  props.width = props.width or POPUP_WIDTH
  local item = sbar.add("item", name, props)
  -- Subscribing every popup child means cursor hovering anywhere in
  -- the popup keeps it open; leaving the last child schedules a close.
  -- `opts.highlightable` adds a subtle bg fill on hover so clickable
  -- rows feel selectable; non-interactive rows (headers, notifications)
  -- omit it.
  item:subscribe("mouse.entered", function()
    open_popup()
    if opts.highlightable then
      item:set({ background = { color = Colors.pill_bg, drawing = "on" } })
    end
  end)
  item:subscribe("mouse.exited", function()
    schedule_close()
    if opts.highlightable then
      item:set({ background = { color = Colors.transparent, drawing = "off" } })
    end
  end)
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

-- Map zmx-session-name → attached? (true when at least one client is
-- connected). Sessions present in claude state but missing from the
-- map are considered detached.
local function read_zmx_attached()
  local handle = io.popen("zmx list 2>/dev/null")
  if not handle then return {} end
  local m = {}
  for line in handle:lines() do
    local name = line:match("name=(%S+)")
    local clients = line:match("clients=(%d+)")
    if name and clients then
      m[name] = tonumber(clients) > 0
    end
  end
  handle:close()
  return m
end

-- Wrap detached zmx session names in parens so the popup distinguishes
-- a live attached session from one that's been detached.
local function display_zmx(name, attached_map)
  if not name or name == "" then return "" end
  if attached_map[name] then return name end
  return "(" .. name .. ")"
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

  -- Each row labels itself with its zmx session (preferred — detached
  -- sessions wrapped in parens) or, when no zmx is persisted, the cwd
  -- basename. Pad the chosen identifier to a single column width.
  local zmx_attached = read_zmx_attached()
  local function row_name(s)
    if s.zmx_session and s.zmx_session ~= "" then
      return display_zmx(s.zmx_session, zmx_attached)
    end
    return basename(s.cwd)
  end

  local max_name = 0
  for _, item in pairs(state.sessions) do
    local n = row_name(item)
    if #n > max_name then max_name = #n end
  end

  for _, entry in ipairs(sorted_sessions()) do
    local s = entry.session
    local ws_str = tostring(s.workspace)
    local prefix = "\u{E861}  " .. pad(row_name(s), max_name)
    local pad_count = math.max(2, TARGET_LABEL_CHARS - utf8.len(prefix) - utf8.len(ws_str))
    local label = prefix .. string.rep(" ", pad_count) .. ws_str
    local short = entry.id:sub(1, 8)
    local child = "agent_sessions." .. short
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
        padding_right = 4,
      },
      -- Prefer focusing the exact aerospace window for this session;
      -- fall back to a workspace switch if window-id wasn't captured
      -- (e.g., a session pinned by an older helper version).
      click_script = (s.window_id and s.window_id ~= ""
        and ("aerospace focus --window-id " .. s.window_id)
        or  ("aerospace workspace " .. s.workspace))
        .. " && sketchybar --set agent_sessions popup.drawing=off",
    }, { highlightable = true })
  end

end

local function aggregate_color()
  local _, top_state = aggregate_state()
  return STATE_COLORS[top_state] or Colors.dim_dark
end

-- Blink the status dot in `color` to call attention to a session
-- state change. Runs for BLINK_DURATION_S, or until superseded by
-- another state change, or until the user hovers the pill — whichever
-- comes first. Each blink call increments a token; the deferred
-- callbacks no-op if their token has been retired.
local BLINK_DURATION_S = 300
local BLINK_INTERVAL_S = 0.6
local blink_token = 0

local function settle_dot()
  blink_token = blink_token + 1   -- stop any active blink
  status_dot:set({ icon = { color = aggregate_color() } })
end

local function start_blink(color)
  blink_token = blink_token + 1
  local my_token = blink_token
  local end_time = os.time() + BLINK_DURATION_S
  local on = false  -- start with the "off" beat so the change is visible

  local function tick()
    if blink_token ~= my_token then return end
    if os.time() >= end_time then
      settle_dot()
      return
    end
    on = not on
    status_dot:set({
      icon = { color = on and color or Colors.dim_dark },
    })
    sbar.exec(string.format("sleep %.2f", BLINK_INTERVAL_S), tick)
  end

  tick()
end

local function repaint_parent()
  local count = aggregate_state()
  count_item:set({
    label = { string = tostring(count) },
  })
  status_dot:set({ icon = { color = aggregate_color() } })
end

-- Subscribe to the helper-fired event. env contains the kv args from
-- `sketchybar --trigger agent_state_change action=write ...`.
parent:subscribe("agent_state_change", function(env)
  local action     = env.action
  local session_id = env.session_id
  local workspace  = env.workspace
  local st         = env.state
  local cwd        = env.cwd

  if not session_id or session_id == "" then
    -- Manual nudge with no session_id — nothing to update in our table.
    return
  end

  local prev = state.sessions[session_id] and state.sessions[session_id].state
  local new_state = nil

  if action == "clear" then
    state.sessions[session_id] = nil
  elseif action == "write" then
    if not workspace or workspace == "" or not st or st == "" then return end
    new_state = st
    state.sessions[session_id] = {
      workspace   = workspace,
      window_id   = env.window_id or "",
      state       = st,
      cwd         = cwd or "",
      zmx_session = env.zmx_session or "",
      updated_at  = os.time(),
    }
  else
    return
  end

  recompute_workspace_state()
  repaint_parent()
  rebuild_popup()

  -- A real state transition for any session triggers the attention
  -- blink; pure metadata refreshes (e.g., a Notification firing while
  -- already in needs-attention) don't.
  if new_state and new_state ~= prev then
    start_blink(STATE_COLORS[new_state] or Colors.dim_dark)
  end

  -- Workspace pills depend on state.workspace_state — fire their event
  -- so they re-paint with the new aggregate.
  sbar.trigger("aerospace_workspace_change")
end)

for _, item in ipairs({ parent, status_dot, count_item }) do
  item:subscribe("mouse.entered", function()
    open_popup()
    -- User saw the pill — drop any active attention blink.
    settle_dot()
  end)
  item:subscribe("mouse.exited", schedule_close)
end

-- Remove any popup children left over from a prior run. Sketchybar's
-- hotload preserves items across reloads, but our `popup_children`
-- table starts empty, so old per-session items would otherwise become
-- orphans (rendered, but not tracked or rebuilt).
sbar.exec(
  [[sketchybar --query bar 2>/dev/null | jq -r '.items[]? | select(startswith("agent_sessions."))']],
  function(out)
    for name in out:gmatch("[^\n]+") do
      sbar.remove(name)
    end
  end
)

-- Initial paint so the pill shows "0" dim grey on startup.
repaint_parent()
