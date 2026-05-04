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

-- Bracket renders the pill chrome. Two states:
--   rest:  2px magenta-cooler underline (workspace-pill focus-bar style)
--   hover: full-height fill in popup_bg so the pill visually merges
--          with the popover that's about to open
local PILL_REST_BG = {
  color         = Colors.magenta,
  corner_radius = 0,
  height        = 2,
  y_offset      = -14,
  border_width  = 0,
}
local PILL_HOVER_BG = {
  color         = Colors.popup_bg,
  corner_radius = 6,
  height        = 28,
  y_offset      = 0,
  border_width  = 0,
}
local pill = sbar.add("bracket", "agent_sessions_pill", {
  parent.name, status_dot.name, count_item.name,
}, { background = PILL_REST_BG })

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

local function add_popup_item(name, props, opts)
  opts = opts or {}
  table.insert(popup_children, name)
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
    local zmx = s.zmx_session or ""
    local detached = zmx ~= "" and not zmx_attached[zmx]

    -- Detached sessions have no terminal window, so the workspace
    -- column is meaningless; render an em-dash instead. Click opens
    -- a fresh ghostty window and re-attaches to the persisted zmx
    -- session on the current desktop.
    local right_col = detached and "\u{2014}" or tostring(s.workspace)
    local label = "\u{E861}  " .. pad(row_name(s), max_name) .. "    " .. right_col
    local short = entry.id:sub(1, 8)
    local child = "agent_sessions." .. short

    local click_script
    if detached then
      -- Mirrors `wft term`'s ghostty invocation: --window-inherit-working-directory=false
      -- avoids a duplicate default window. zmx takes the prefix-stripped
      -- name (`smm.0`, not `d.smm.0`) — the prefix is auto-applied from
      -- $ZMX_SESSION_PREFIX, which login won't carry, so we pass the
      -- short form. Absolute zmx path because login's PATH doesn't
      -- include /opt/homebrew/bin.
      -- Helper now writes zmx_short directly. For pins from older helper
      -- versions, strip up to the first dot as a heuristic — matches
      -- the "<prefix>.<name>" convention zmx uses (e.g. "d.smm.0" →
      -- "smm.0"). Refreshes properly on next hook fire.
      local zmx_arg = s.zmx_short
      if not zmx_arg or zmx_arg == "" then
        zmx_arg = zmx:match("^[^.]+%.(.+)$") or zmx
      end
      click_script = string.format(
        "open -na Ghostty.app --args --window-inherit-working-directory=false "
        .. "-e /opt/homebrew/bin/zmx a %s",
        zmx_arg
      ) .. " && sketchybar --set agent_sessions popup.drawing=off"
    elseif s.window_id and s.window_id ~= "" then
      click_script = "aerospace focus --window-id " .. s.window_id
        .. " && sketchybar --set agent_sessions popup.drawing=off"
    else
      click_script = "aerospace workspace " .. s.workspace
        .. " && sketchybar --set agent_sessions popup.drawing=off"
    end

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
      click_script = click_script,
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

-- Pulse loop for popup rows whose session is in `running` state. The
-- dot icon alternates between full state color and dim_dark so the row
-- visibly "breathes" while the agent is working. Loop runs only while
-- at least one running session exists; tick checks state.sessions
-- on every iteration so dynamic state changes are reflected without
-- a separate restart path.
local PULSE_INTERVAL_S = 0.7
local pulse_alive = false

local function any_running()
  for _, s in pairs(state.sessions) do
    if s.state == "running" then return true end
  end
  return false
end

local function paint_running_rows(on)
  for id, s in pairs(state.sessions) do
    if s.state == "running" then
      local short = id:sub(1, 8)
      sbar.set("agent_sessions." .. short, {
        icon = { color = on and STATE_COLORS.running or Colors.dim_dark },
      })
    end
  end
end

local function start_pulse_if_needed()
  if pulse_alive or not any_running() then return end
  pulse_alive = true
  local on = true

  local function tick()
    if not any_running() then
      pulse_alive = false
      return
    end
    on = not on
    paint_running_rows(on)
    sbar.exec(string.format("sleep %.2f", PULSE_INTERVAL_S), tick)
  end

  tick()
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
      zmx_short   = env.zmx_short or "",
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

  -- Wake (or keep alive) the popup-row pulse for any running sessions.
  start_pulse_if_needed()

  -- Workspace pills depend on state.workspace_state — fire their event
  -- so they re-paint with the new aggregate.
  sbar.trigger("aerospace_workspace_change")
end)

for _, item in ipairs({ parent, status_dot, count_item }) do
  item:subscribe("mouse.entered", function()
    open_popup()
    -- User saw the pill — drop any active attention blink.
    settle_dot()
    pill:set({ background = PILL_HOVER_BG })
  end)
  item:subscribe("mouse.exited", function()
    schedule_close()
    pill:set({ background = PILL_REST_BG })
  end)
end

-- Remove any popup children left over from a prior run. Sketchybar's
-- hotload preserves items across reloads, but our `popup_children`
-- table starts empty, so old per-session items would otherwise become
-- orphans (rendered, but not tracked or rebuilt). After cleanup,
-- restore state.sessions from on-disk pin files (written by the helper
-- on every hook fire) so the popup shows the previously-known agents
-- immediately after a sketchybar reload — rather than starting empty
-- and waiting for each session to fire its next hook.
local PIN_DIR = (os.getenv("XDG_STATE_HOME") or (os.getenv("HOME") .. "/.local/state"))
  .. "/sketchybar/sessions"

local function restore_sessions()
  local cmd = string.format(
    "find %q -maxdepth 1 -name '*.json' -print0 2>/dev/null | "
    .. "xargs -0 jq -r '[.session_id, (.workspace // \"\"), (.window_id // \"\"), "
    .. "(.state // \"\"), (.cwd // \"\"), (.zmx_session // \"\"), "
    .. "(.zmx_short // \"\"), ((.updated_at // 0)|tostring)] | @tsv' 2>/dev/null",
    PIN_DIR
  )
  local handle = io.popen(cmd)
  if not handle then return end
  for line in handle:lines() do
    local sid, ws, win, st, cwd, zmx, zsh, ts =
      line:match("^([^\t]*)\t([^\t]*)\t([^\t]*)\t([^\t]*)\t([^\t]*)\t([^\t]*)\t([^\t]*)\t([^\t]*)$")
    if sid and sid ~= "" and ws ~= "" and st ~= "" then
      state.sessions[sid] = {
        workspace   = ws,
        window_id   = win,
        state       = st,
        cwd         = cwd,
        zmx_session = zmx,
        zmx_short   = zsh,
        updated_at  = tonumber(ts) or 0,
      }
    end
  end
  handle:close()
end

sbar.exec(
  [[sketchybar --query bar 2>/dev/null | jq -r '.items[]? | select(startswith("agent_sessions."))']],
  function(out)
    for name in out:gmatch("[^\n]+") do
      sbar.remove(name)
    end
    restore_sessions()
    recompute_workspace_state()
    repaint_parent()
    rebuild_popup()
    start_pulse_if_needed()
    sbar.trigger("aerospace_workspace_change")
  end
)

-- Initial paint so the pill shows "0" dim grey before the async
-- restore above completes.
repaint_parent()
