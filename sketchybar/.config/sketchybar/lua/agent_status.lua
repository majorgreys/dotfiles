-- agent_status.lua — agent_sessions parent pill + popup, push-driven.
--
-- The sketchybar-set-state shell helper is invoked from Claude Code
-- hooks (UserPromptSubmit, Stop, Notification, SessionEnd). It triggers
-- the `agent_state_change` sketchybar event with these args:
--   action       = "write" | "clear"
--   session_id   = "<uuid>"
--   state        = "submitted" | "running" | "tooling" | "needs-attention" | "idle" | "error" | "stale"
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
  tooling             = Colors.magenta,
  running             = Colors.green,
  submitted           = Colors.accent_bg,
  idle                = Colors.dim_dark,
  stale               = Colors.dim_dark,
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

-- The parent pill (robot icon + count) doubles as the trigger for the
-- overflow popup. Hovering anywhere on the parent or count_item opens
-- a vertical dropdown listing every session that isn't shown inline
-- (idle, detached, stale needs-attention). MacBooks with a camera
-- notch have limited horizontal bar real estate; the popup keeps the
-- full list available without claiming inline space.
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
      corner_radius = 6,
      border_width  = 1,
      border_color  = Colors.popup_border,
    },
    horizontal = false,
    align      = "right",
    y_offset   = 4,
  },
})

-- 2px magenta underline applied to session bar items in `needs-attention`
-- state. Marks the agent that is waiting for a response; replaces the
-- former parent-pill bracket underline.
local NEEDS_ATTENTION_BG = {
  color         = Colors.magenta,
  corner_radius = 0,
  height        = 2,
  y_offset      = -14,
  border_width  = 0,
}

-- 2px yellow underline applied while the cursor is over a session.
-- Same shape as NEEDS_ATTENTION_BG so hover and rest-attention states
-- coexist visually (single bg slot — exit restores the rest state).
local HOVER_BG = {
  color         = Colors.yellow,
  corner_radius = 0,
  height        = 2,
  y_offset      = -14,
  border_width  = 0,
}

-- Rest-state bg for sessions without a needs-attention underline.
-- Transparent + full-height so the entire pill column is a hover
-- surface (sketchybar uses the drawn bg rect for mouse hit-testing
-- when drawing=on — a 2px underline alone has only a 2px hover zone).
local REST_BG_NONE = {
  drawing       = true,
  color         = Colors.transparent,
  height        = 28,
  y_offset      = 0,
  corner_radius = 0,
  border_width  = 0,
}

-- needs-attention underline is suppressed for sessions whose last
-- recorded state-change is older than this. State files persist the
-- LAST hook fire, so abandoned sessions sit in `needs-attention`
-- indefinitely; the TTL stops every dormant session from drawing the
-- user's eye.
local NEEDS_ATTENTION_TTL_S = 300
-- If a session has not reported any state change for this long, demote
-- it to stale for aggregate counts / workspace urgency. Unknown-PID
-- sessions are only removed completely after the longer hard TTL below.
local STALE_TTL_S = 60 * 60
local HARD_STALE_TTL_S = 24 * 60 * 60

-- 2px green underline for sessions in `running` state. The pulse loop
-- (start_pulse_if_needed) fades the alpha between BRIGHT and DIM with
-- sketchybar's `--animate sin` interpolation — the line never fully
-- disappears, just breathes in intensity. Slower + never-off = more
-- calming than a hard on/off blink.
local RUNNING_BG_BRIGHT_HEX = "0xff44bc44"  -- Colors.green, full alpha
local RUNNING_BG_DIM_HEX    = "0x4044bc44"  -- ~25% alpha
local RUNNING_BG = {
  color         = Colors.green,
  corner_radius = 0,
  height        = 2,
  y_offset      = -14,
  border_width  = 0,
}
-- One direction of the fade, in seconds. Total cycle = 2 * this.
local PULSE_HALF_S = 1.2

-- Cache of the currently-focused aerospace workspace, kept in sync via
-- aerospace_workspace_change. Used in agent_state_change to suppress
-- the needs-attention underline when a session transitions while its
-- workspace is already in focus (the user is already there, no
-- attention prompt needed).
local focused_workspace = nil

-- Track current per-session bar items so we can wipe them cleanly on
-- rebuild. sketchybar has no "remove by prefix" lua primitive.
local session_items = {}
local overflow_children = {}

local function clear_session_items()
  for _, name in ipairs(session_items) do
    sbar.remove(name)
  end
  session_items = {}
end

local function clear_overflow_children()
  for _, name in ipairs(overflow_children) do
    sbar.remove(name)
  end
  overflow_children = {}
end

local function add_session_item(name, props)
  table.insert(session_items, name)
  return sbar.add("item", name, props)
end

local function add_overflow_child(name, props)
  table.insert(overflow_children, name)
  return sbar.add("item", name, props)
end

local function item_suffix(id)
  return tostring(id or ""):gsub("[^%w_]", "_"):sub(1, 8)
end

-- Hover-driven popup with a small close grace so a cursor sweep from
-- the parent pill into the popup doesn't slam it shut mid-traversal.
local OVERFLOW_CLOSE_DELAY_S = 0.6
local overflow_close_token = 0

local function open_overflow_popup()
  overflow_close_token = overflow_close_token + 1
  parent:set({ popup = { drawing = "on" } })
end

local function schedule_close_overflow_popup()
  overflow_close_token = overflow_close_token + 1
  local my = overflow_close_token
  sbar.exec(string.format("sleep %.2f", OVERFLOW_CLOSE_DELAY_S), function()
    if overflow_close_token == my then
      parent:set({ popup = { drawing = "off" } })
    end
  end)
end

-- Hovering the robot icon or the count both open the popup. The
-- in-popup rows also subscribe to mouse.entered/exited in
-- rebuild_session_items so a cursor sweep across rows keeps the popup
-- open.
for _, item in ipairs({ parent, count_item }) do
  item:subscribe("mouse.entered", open_overflow_popup)
  item:subscribe("mouse.exited", schedule_close_overflow_popup)
end

-- Pin file directory; declared early so dedupe_sessions can remove
-- loser files. restore_sessions further down references the same path.
local PIN_DIR = (os.getenv("XDG_STATE_HOME") or (os.getenv("HOME") .. "/.local/state"))
  .. "/sketchybar/sessions"

local function pid_alive(pid)
  if not pid or pid == 0 then return true end
  local result = os.execute(string.format("kill -0 %d 2>/dev/null", pid))
  return result == 0 or result == true
end

local function effective_state(s, now)
  local st = s.state or "idle"
  local age = now - (s.updated_at or 0)
  if st == "needs-attention" and age > NEEDS_ATTENTION_TTL_S then
    return "idle"
  end
  if age > STALE_TTL_S and st ~= "running" and st ~= "tooling" then
    return "stale"
  end
  return st
end

local function is_actionable_state(st)
  return st == "error"
    or st == "needs-attention"
    or st == "tooling"
    or st == "running"
    or st == "submitted"
end

local function prune_dead_sessions()
  local now = os.time()
  for id, s in pairs(state.sessions) do
    local age = now - (s.updated_at or 0)
    local missing_pid = s.agent_pid and s.agent_pid ~= 0 and not pid_alive(s.agent_pid)
    local ancient_unknown = (not s.agent_pid or s.agent_pid == 0) and age > HARD_STALE_TTL_S
    if missing_pid or ancient_unknown then
      state.sessions[id] = nil
      os.remove(PIN_DIR .. "/" .. id .. ".json")
    end
  end
end

local function compact_meta(s)
  local parts = {}
  if s.workstream and s.workstream ~= "" then table.insert(parts, s.workstream) end
  if s.claimed_issue and s.claimed_issue ~= "" then table.insert(parts, s.claimed_issue) end
  local pending = tonumber(s.todo_pending) or 0
  local active = tonumber(s.todo_in_progress) or 0
  if pending + active > 0 then table.insert(parts, string.format("todo %d/%d", active, pending + active)) end
  local workers = (tonumber(s.venom_active) or 0) + (tonumber(s.venom_waiting) or 0)
  if workers > 0 then table.insert(parts, "vm " .. tostring(workers)) end
  local gates = tonumber(s.human_gates) or 0
  if gates > 0 then table.insert(parts, "gate " .. tostring(gates)) end
  if #parts == 0 then return "" end
  return " · " .. table.concat(parts, " · ")
end

-- Group key for dedupe: zmx session if present, else cwd. Multiple
-- claude processes in the same zmx all share the same key, so the
-- newest one supersedes the rest. Different cwds in the same zmx still
-- collapse — assumption: one claude per terminal at a time. If you
-- ever want parallel claudes in one zmx to coexist, swap to a
-- (zmx, cwd) tuple key.
local function group_key(s)
  if s.zmx_session and s.zmx_session ~= "" then
    return "zmx:" .. s.zmx_session
  end
  return "cwd:" .. (s.cwd or "")
end

-- Drop pin files for sessions that lose to a sibling in the same group.
-- Mutates state.sessions in place. Called after restore and after every
-- agent_state_change write.
local function dedupe_sessions()
  local winner = {}
  for id, s in pairs(state.sessions) do
    local key = group_key(s)
    local t = s.updated_at or 0
    local cur = winner[key]
    if not cur or t > cur.updated_at then
      winner[key] = { id = id, updated_at = t }
    end
  end
  local keep = {}
  for _, w in pairs(winner) do keep[w.id] = true end
  for id, _ in pairs(state.sessions) do
    if not keep[id] then
      state.sessions[id] = nil
      os.remove(PIN_DIR .. "/" .. id .. ".json")
    end
  end
end

-- Recompute state.workspace_state from state.sessions.
local function recompute_workspace_state()
  prune_dead_sessions()
  local ws_state = {}
  local now = os.time()
  for _, s in pairs(state.sessions) do
    local st = effective_state(s, now)
    local cur = ws_state[s.workspace]
    if not cur or (state.urgency[st] or 0) > (state.urgency[cur] or 0) then
      ws_state[s.workspace] = st
    end
  end
  state.workspace_state = ws_state
end

-- Sort key: zmx session name, else cwd basename, lowercased so
-- "Aerospace" sorts next to "aerospace". Stable across state changes
-- so pills don't shuffle position when an agent transitions
-- running/needs-attention/idle.
local function sort_key(s)
  local name = (s.zmx_session and s.zmx_session ~= "" and s.zmx_session)
    or (s.cwd or "")
  return string.lower(name)
end

-- Sort sessions descending by name so the bar reads alphabetically
-- left-to-right (sketchybar prepends right-positioned items, so the
-- FIRST added lands rightmost).
local function sorted_sessions()
  local list = {}
  for id, s in pairs(state.sessions) do
    table.insert(list, { id = id, session = s })
  end
  table.sort(list, function(a, b)
    local ka, kb = sort_key(a.session), sort_key(b.session)
    if ka ~= kb then return ka > kb end
    return a.id > b.id
  end)
  return list
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
    local name = line:match("name=(%S+)") or line:match("^(%S+)%s+pid=")
    local clients = line:match("clients=(%d+)")
    if name and clients then
      m[name] = tonumber(clients) > 0
    end
  end
  handle:close()
  return m
end

-- Wrap detached zmx session names in parens so the popup distinguishes
-- a live attached session from one that's been detached. `lookup_key`
-- (optional) is the name used to query `attached_map` — pass when
-- `display` is the prefix-stripped short name but the map keys are the
-- full names that `zmx l` reports.
local function display_zmx(display, attached_map, lookup_key)
  if not display or display == "" then return "" end
  local key = (lookup_key and lookup_key ~= "") and lookup_key or display
  if attached_map[key] then return display end
  return "(" .. display .. ")"
end


-- Highest-urgency state across all sessions. Drives the status dot
-- color in the menu-bar pill. Returns the count for the label.
local function aggregate_state()
  local count = 0
  local top_state, top_rank = "idle", -1
  local now = os.time()
  for _, s in pairs(state.sessions) do
    local st = effective_state(s, now)
    local actionable = is_actionable_state(st)
    if st == "needs-attention" then
      actionable = (now - (s.updated_at or 0)) <= NEEDS_ATTENTION_TTL_S
        and (s.viewed_at or 0) < (s.updated_at or 0)
    end
    if actionable then count = count + 1 end
    local ranked_state = actionable and st or "idle"
    local rank = state.urgency[ranked_state] or 0
    if rank > top_rank then
      top_rank, top_state = rank, ranked_state
    end
  end
  return count, top_state
end

-- Build the shell command that handles a click on a session item.
-- Detached sessions reopen a fresh Ghostty + zmx attach; live sessions
-- focus the window/workspace via aerospace.
local function make_click_script(s, detached)
  local zmx = s.zmx_session or ""
  if detached then
    -- Mirrors `wft term` / `ghostty::spawn_window`: open a fresh Ghostty
    -- window attached to the session's zmx. Key flags:
    --   --window-inherit-working-directory=false  so --working-directory wins
    --   --working-directory=<cwd>                 land in the project dir
    --   -e zmx attach <session>                   reattach to the persisted zmx
    -- Use the FULL zmx session name (e.g. "d.dotfiles.0") because the
    -- Ghostty login shell won't have $ZMX_SESSION_PREFIX set. Using the
    -- short name would create a new empty session instead of reattaching.
    local cwd = s.cwd ~= "" and s.cwd or os.getenv("HOME") or "/tmp"
    local esc_cwd = cwd:gsub("'", "'\\''")
    return string.format(
      "open -n -a Ghostty --args"
      .. " --window-inherit-working-directory=false"
      .. " --working-directory='%s'"
      .. " -e zmx attach %s",
      esc_cwd, zmx
    )
  elseif s.window_id and s.window_id ~= "" then
    return "aerospace focus --window-id " .. s.window_id
  end
  return "aerospace workspace " .. s.workspace
end

local function rebuild_session_items()
  prune_dead_sessions()
  clear_session_items()
  clear_overflow_children()

  -- Each session labels itself with its zmx session (preferred —
  -- detached sessions wrapped in parens) or, when no zmx is persisted,
  -- the cwd basename. Prefer zmx_short (helper-computed: zmx_session
  -- with $ZMX_SESSION_PREFIX stripped — e.g. "d.claude.ops.fedf" →
  -- "claude.ops.fedf") so the bar isn't cluttered with the namespace
  -- prefix users already know belongs to them.
  local zmx_attached = read_zmx_attached()
  local function row_name(s)
    local display = (s.zmx_short and s.zmx_short ~= "" and s.zmx_short)
      or (s.zmx_session and s.zmx_session ~= "" and s.zmx_session)
    local base
    if display then
      -- display_zmx wraps in parens for detached. Pass zmx_session to
      -- the attached-map lookup since `zmx l` reports full names.
      base = display_zmx(display, zmx_attached, s.zmx_session)
    else
      base = basename(s.cwd)
    end
    return base .. compact_meta(s)
  end

  -- Split sessions into inline (currently active) and overflow
  -- (everything else). "Currently active" means attached AND either:
  --   - state == running, OR
  --   - state == needs-attention with a fresh, unviewed hook fire
  -- (the same predicate that drives the magenta underline). Stale or
  -- already-viewed needs-attention falls to overflow so the bar shows
  -- only the agents actually waiting on the user right now.
  local now = os.time()
  local inline = {}
  local overflow = {}
  for _, entry in ipairs(sorted_sessions()) do
    local s = entry.session
    local zmx = s.zmx_session or ""
    local detached = zmx ~= "" and not zmx_attached[zmx]
    local fresh = (now - (s.updated_at or 0)) <= NEEDS_ATTENTION_TTL_S
    local unviewed = (s.viewed_at or 0) < (s.updated_at or 0)
    local st = effective_state(s, now)
    local active = st == "submitted"
      or st == "running"
      or st == "tooling"
      or st == "error"
      or (st == "needs-attention" and fresh and unviewed)
    local record = { entry = entry, s = s, detached = detached }
    if active and not detached then
      table.insert(inline, record)
    else
      table.insert(overflow, record)
    end
  end

  -- Inline pills (position=right). Order matters: sketchybar prepends
  -- right-positioned items, so the FIRST add ends up rightmost (closest
  -- to the parent pill). We iterate in sorted order so most-urgent sits
  -- adjacent to the parent.
  for i, r in ipairs(inline) do
    local s = r.s
    local label = row_name(s)
    local short = item_suffix(r.entry.id)
    local child = "agent_sessions." .. short

    -- The rightmost session (first in sorted order, sitting next to
    -- the parent pill) gets extra label padding so there's a visible
    -- gap before the robot icon.
    local is_rightmost = i == 1
    local label_right_pad = is_rightmost and 16 or 8

    -- needs-attention underline applies only when:
    --   1. session state is needs-attention,
    --   2. last state-change is within NEEDS_ATTENTION_TTL_S, and
    --   3. user hasn't focused this session's workspace since the
    --      state-change (viewed_at < updated_at).
    -- (3) clears the underline as soon as the user switches into the
    -- agent's workspace; the next Stop hook re-arms it. (detached is
    -- already filtered out at split time.)
    local fresh = (now - (s.updated_at or 0)) <= NEEDS_ATTENTION_TTL_S
    local unviewed = (s.viewed_at or 0) < (s.updated_at or 0)
    local st = effective_state(s, now)
    local attention_active = st == "needs-attention" and fresh and unviewed
    -- Running/tooling/submitted sessions get a live underline as their rest
    -- bg so the pulse loop only animates color for running/tooling phases
    -- (geometry stays put). Hover/exit still swap to/from HOVER_BG.
    local rest_bg
    if attention_active then
      rest_bg = NEEDS_ATTENTION_BG
    elseif st == "running" or st == "tooling" or st == "submitted" then
      rest_bg = {
        color         = STATE_COLORS[st] or Colors.green,
        corner_radius = 0,
        height        = 2,
        y_offset      = -14,
        border_width  = 0,
      }
    else
      rest_bg = REST_BG_NONE
    end

    local item = add_session_item(child, {
      position = "right",
      icon = { drawing = false },
      label = {
        string        = label,
        font          = Fonts.popup,
        color         = Colors.fg,
        padding_left  = 6,
        padding_right = label_right_pad,
      },
      background   = rest_bg,
      click_script = make_click_script(s, false),
    })

    item:subscribe("mouse.entered", function()
      item:set({ background = HOVER_BG })
    end)
    item:subscribe("mouse.exited", function()
      item:set({ background = rest_bg })
    end)
  end

  -- Overflow popup children (vertical list under the parent pill). Each
  -- row clicks through to the same workspace/spawn action as an inline
  -- pill. Detached rows get parens via display_zmx in row_name.
  for _, r in ipairs(overflow) do
    local s = r.s
    local short = item_suffix(r.entry.id)
    local child = "agent_sessions_overflow." .. short
    local row_label = row_name(s)
    local row = add_overflow_child(child, {
      position = "popup." .. parent.name,
      icon = { drawing = false },
      label = {
        string        = row_label,
        font          = Fonts.popup,
        color         = r.detached and Colors.dim or Colors.fg,
        padding_left  = 12,
        padding_right = 12,
      },
      click_script = make_click_script(s, r.detached),
    })
    -- Hovering popup rows keeps the popup open and provides a subtle
    -- highlight. Leaving the row schedules a close like the parent.
    row:subscribe("mouse.entered", function()
      open_overflow_popup()
      row:set({ background = { color = Colors.pill_bg, drawing = "on" } })
    end)
    row:subscribe("mouse.exited", function()
      schedule_close_overflow_popup()
      row:set({ background = { color = Colors.transparent, drawing = "off" } })
    end)
  end

  -- If there's nothing to overflow, make sure the popup isn't held
  -- open by a stale draw flag.
  if #overflow == 0 then
    parent:set({ popup = { drawing = "off" } })
  end
end

local function repaint_parent()
  local count, top_state = aggregate_state()
  local color = STATE_COLORS[top_state] or Colors.fg
  parent:set({ icon = { color = color } })
  count_item:set({
    label = { string = tostring(count), color = count > 0 and Colors.fg or Colors.dim_dark },
  })
end

-- Calm heartbeat for inline `running` sessions. Each tick uses
-- sketchybar's --animate sin to smoothly fade the underline alpha
-- toward the next target color over PULSE_HALF_S, then schedules
-- itself for another tick after the fade finishes. The bg geometry
-- stays constant (set by rebuild_session_items); only color is
-- animated, so the underline never disappears — it just breathes.
local pulse_alive = false

local function any_running_inline(zmx_attached)
  for _, s in pairs(state.sessions) do
    local st = effective_state(s, os.time())
    if st == "running" or st == "tooling" then
      local zmx = s.zmx_session or ""
      local detached = zmx ~= "" and not zmx_attached[zmx]
      if not detached then return true end
    end
  end
  return false
end

local function animate_pulse_color(name, hex, frames)
  sbar.exec(string.format(
    "sketchybar --animate sin %d --set %s background.color=%s",
    frames, name, hex
  ))
end

local function paint_running_pulse(target_hex, frames)
  local zmx_attached = read_zmx_attached()
  for id, s in pairs(state.sessions) do
    local st = effective_state(s, os.time())
    if st == "running" or st == "tooling" then
      local zmx = s.zmx_session or ""
      local detached = zmx ~= "" and not zmx_attached[zmx]
      if not detached then
        local name = "agent_sessions." .. item_suffix(id)
        animate_pulse_color(name, target_hex, frames)
      end
    end
  end
end

local function start_pulse_if_needed()
  if pulse_alive then return end
  if not any_running_inline(read_zmx_attached()) then return end
  pulse_alive = true
  -- rebuild_session_items leaves running pills at full BRIGHT alpha,
  -- so the first tick should fade toward DIM (bright=true → not bright
  -- = false → DIM).
  local bright = true
  local frames = math.floor(PULSE_HALF_S * 60)
  local function tick()
    if not any_running_inline(read_zmx_attached()) then
      pulse_alive = false
      return
    end
    bright = not bright
    paint_running_pulse(bright and RUNNING_BG_BRIGHT_HEX or RUNNING_BG_DIM_HEX, frames)
    sbar.exec(string.format("sleep %.2f", PULSE_HALF_S), tick)
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
    -- Preserve viewed_at across hook updates. The aerospace handler
    -- below sets it when the user focuses a workspace; resetting it
    -- here would make the needs-attention underline never clear.
    -- Bonus rule: if the session is on the workspace currently in
    -- focus, treat the new state as already viewed so a transition
    -- to needs-attention doesn't flash an underline at the user when
    -- they're already looking at that workspace.
    local prev_session = state.sessions[session_id]
    local now = os.time()
    local prior_viewed = prev_session and prev_session.viewed_at or 0
    local viewed_at = prior_viewed
    if focused_workspace and tostring(workspace) == tostring(focused_workspace) then
      viewed_at = now
    end
    state.sessions[session_id] = {
      workspace   = workspace,
      window_id   = env.window_id or "",
      state       = st,
      cwd         = cwd or "",
      zmx_session      = env.zmx_session or "",
      zmx_short        = env.zmx_short or "",
      agent            = env.agent or "",
      agent_pid        = tonumber(env.agent_pid) or 0,
      tool_name        = env.tool_name or "",
      workstream       = env.workstream or "",
      claimed_issue    = env.claimed_issue or "",
      claimed_title    = env.claimed_title or "",
      todo_pending     = tonumber(env.todo_pending) or 0,
      todo_in_progress = tonumber(env.todo_in_progress) or 0,
      venom_active     = tonumber(env.venom_active) or 0,
      venom_waiting    = tonumber(env.venom_waiting) or 0,
      venom_failed     = tonumber(env.venom_failed) or 0,
      human_gates      = tonumber(env.human_gates) or 0,
      updated_at       = now,
      viewed_at        = viewed_at,
    }
  else
    return
  end

  -- Any newer hook fire from a sibling in the same zmx/cwd group means
  -- the older session is dead — its claude process never reported back.
  -- Evict losers and delete their pin files.
  dedupe_sessions()
  recompute_workspace_state()
  repaint_parent()
  rebuild_session_items()
  start_pulse_if_needed()

  -- Workspace focus changes arrive from AeroSpace directly. Do not fan out
  -- synthetic workspace-change events here: each workspace pill falls back to
  -- querying AeroSpace when FOCUSED_WORKSPACE is absent, so doing this on
  -- every agent update can burn CPU.
end)

-- Mark sessions on the focused workspace as viewed so their
-- needs-attention underline disappears. AeroSpace's
-- exec-on-workspace-change supplies env.FOCUSED_WORKSPACE; the internal
-- triggers fired from agent_state_change above don't, so we filter to
-- real focus changes only.
parent:subscribe("aerospace_workspace_change", function(env)
  local ws = env.FOCUSED_WORKSPACE
  if not ws or ws == "" then return end
  focused_workspace = ws
  local now = os.time()
  local touched = false
  for _, s in pairs(state.sessions) do
    if tostring(s.workspace) == tostring(ws)
        and s.state == "needs-attention"
        and (s.viewed_at or 0) < (s.updated_at or 0) then
      s.viewed_at = now
      touched = true
    end
  end
  if touched then
    rebuild_session_items()
  end
end)

-- Remove any per-session bar items left over from a prior run.
-- Sketchybar's hotload preserves items across reloads, but our
-- `session_items` table starts empty, so old items would otherwise
-- become orphans (rendered, but not tracked or rebuilt). After cleanup,
-- restore state.sessions from on-disk pin files (written by the helper
-- on every hook fire) so the bar shows the previously-known agents
-- immediately after a sketchybar reload — rather than starting empty
-- and waiting for each session to fire its next hook. PIN_DIR is
-- declared up-top so dedupe_sessions can also see it.

local function restore_sessions()
  state.sessions = {}
  local cmd = string.format(
    "find %q -maxdepth 1 -name '*.json' -print0 2>/dev/null | "
    .. "xargs -0 jq -r '[.session_id, (.workspace // \"\"), (.window_id // \"\"), "
    .. "(.state // \"\"), (.cwd // \"\"), (.zmx_session // \"\"), "
    .. "(.zmx_short // \"\"), (.agent // \"\"), ((.updated_at // 0)|tostring), "
    .. "((.agent_pid // 0)|tostring), (.tool_name // \"\"), (.workstream // \"\"), "
    .. "(.claimed_issue // \"\"), (.claimed_title // \"\"), ((.todo_pending // 0)|tostring), "
    .. "((.todo_in_progress // 0)|tostring), ((.venom_active // 0)|tostring), "
    .. "((.venom_waiting // 0)|tostring), ((.venom_failed // 0)|tostring), "
    .. "((.human_gates // 0)|tostring)] | @tsv' 2>/dev/null",
    PIN_DIR
  )
  local handle = io.popen(cmd)
  if not handle then return end
  for line in handle:lines() do
    local fields = {}
    for value in (line .. "\t"):gmatch("([^\t]*)\t") do
      table.insert(fields, value)
    end
    local sid, ws, win, st, cwd, zmx, zsh, agent, ts, pid =
      fields[1], fields[2], fields[3], fields[4], fields[5], fields[6], fields[7], fields[8], fields[9], fields[10]
    if sid and sid ~= "" and ws ~= "" and st ~= "" then
      local agent_pid = tonumber(pid) or 0
      if pid_alive(agent_pid) then
        state.sessions[sid] = {
          workspace        = ws,
          window_id        = win,
          state            = st,
          cwd              = cwd,
          zmx_session      = zmx,
          zmx_short        = zsh,
          agent            = agent,
          agent_pid        = agent_pid,
          tool_name        = fields[11] or "",
          workstream       = fields[12] or "",
          claimed_issue    = fields[13] or "",
          claimed_title    = fields[14] or "",
          todo_pending     = tonumber(fields[15]) or 0,
          todo_in_progress = tonumber(fields[16]) or 0,
          venom_active     = tonumber(fields[17]) or 0,
          venom_waiting    = tonumber(fields[18]) or 0,
          venom_failed     = tonumber(fields[19]) or 0,
          human_gates      = tonumber(fields[20]) or 0,
          updated_at       = tonumber(ts) or 0,
        }
      else
        -- Agent process is gone; drop the pin file so this session
        -- doesn't get restored on the next sketchybar reload either.
        os.remove(PIN_DIR .. "/" .. sid .. ".json")
      end
    end
  end
  handle:close()
  dedupe_sessions()
end

local function log_debug(msg)
  local f = io.open("/tmp/sketchybar-agent-status.log", "a")
  if not f then return end
  f:write(os.date("%Y-%m-%d %H:%M:%S"), " ", tostring(msg), "\n")
  f:close()
end

local function remove_known_session_items()
  for id, _ in pairs(state.sessions) do
    pcall(sbar.remove, "agent_sessions." .. item_suffix(id))
    pcall(sbar.remove, "agent_sessions_overflow." .. item_suffix(id))
  end
  session_items = {}
  overflow_children = {}
end

local function refresh_from_pins(trigger_focus)
  restore_sessions()
  remove_known_session_items()
  local restored = 0
  for _, _ in pairs(state.sessions) do restored = restored + 1 end
  recompute_workspace_state()
  repaint_parent()
  rebuild_session_items()
  start_pulse_if_needed()
  log_debug("refresh restored=" .. tostring(restored))
end

local function safe_refresh_from_pins()
  local ok, err = pcall(refresh_from_pins)
  if not ok then log_debug("ERROR " .. tostring(err)) end
end

local function schedule_pin_refresh(delay_s)
  sbar.exec("sleep " .. tostring(delay_s or 10), function()
    safe_refresh_from_pins()
    schedule_pin_refresh(10)
  end)
end
schedule_pin_refresh(1)

-- Seed `focused_workspace` so the "no underline if already focused"
-- rule works on the first hook fire after sketchybar startup. Without
-- this, focused_workspace is nil until the user actually switches
-- workspaces.
sbar.exec("aerospace list-workspaces --focused 2>/dev/null", function(out)
  out = (out or ""):gsub("%s+$", "")
  if out ~= "" then focused_workspace = out end
end)

-- Initial paint so the pill shows "0" dim grey before the async
-- restore above completes.
repaint_parent()
