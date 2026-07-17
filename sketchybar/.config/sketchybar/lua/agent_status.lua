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
-- We update an in-memory sessions table, prune stale duplicate records,
-- then rebuild the popup children. No filesystem state for the dropdown.
-- A small per-session
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

-- Remove the legacy separate count item when hotloading this config.
pcall(sbar.remove, "agent_sessions_count")

-- The pizza pill is the trigger for the session popup. Hovering opens a
-- vertical dropdown listing EVERY session — active and idle alike.
-- MacBooks with a camera notch have limited horizontal bar real estate,
-- so nothing is drawn inline; the popup holds the full list and the
-- pizza matches the `pizza.*` zmx/pi session prefix.
local parent = sbar.add("item", "agent_sessions", {
  position = "right",
  icon = {
    string        = "󰐉",
    font          = Fonts.icon,
    color         = Colors.fg,
    padding_left  = 8,
    padding_right = 8,
  },
  label = { drawing = false },
  -- Keep the background box and border geometry fixed in both states. Pinning
  -- changes their colors only, so the pizza never shifts horizontally.
  background = {
    color         = Colors.transparent,
    corner_radius = 4,
    border_width  = 1,
    border_color  = Colors.transparent,
    height        = 26,
    drawing       = "on",
  },
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

-- needs-attention underline is suppressed for sessions whose last
-- recorded state-change is older than this. State files persist the
-- LAST hook fire, so abandoned sessions sit in `needs-attention`
-- indefinitely; the TTL stops every dormant session from drawing the
-- user's eye.
local NEEDS_ATTENTION_TTL_S = 300
-- If a session has not reported any state change for this long, demote
-- it to stale for aggregate status. Unknown-PID
-- sessions are only removed completely after the longer hard TTL below.
local STALE_TTL_S = 60 * 60
local HARD_STALE_TTL_S = 24 * 60 * 60

-- Bright/dim green the parent pizza icon fades between while any
-- session is running/tooling. The pulse loop (start_pulse_if_needed)
-- interpolates icon.color between these with sketchybar's `--animate
-- sin` — the color never fully drops out, it just breathes in
-- intensity. Slower + never-off = more calming than a hard on/off blink.
local RUNNING_BG_BRIGHT_HEX = "0xff44bc44"  -- Colors.green, full alpha
local RUNNING_BG_DIM_HEX    = "0x4044bc44"  -- ~25% alpha
-- One direction of the fade, in seconds. Total cycle = 2 * this.
local PULSE_HALF_S = 1.2

-- Cache of the currently-focused aerospace workspace, kept in sync via
-- aerospace_workspace_change. Used in agent_state_change to suppress
-- the needs-attention underline when a session transitions while its
-- workspace is already in focus (the user is already there, no
-- attention prompt needed).
local focused_workspace = nil

-- Cache of `zmx list` output parsed into session-name -> attached?. The
-- bar CANNOT call io.popen synchronously: SbarLua installs
-- signal(SIGCHLD, SIG_IGN) process-wide, under which pclose()'s wait4
-- blocks until EVERY child has exited. SbarLua perpetually keeps other
-- forked children alive (the pin-refresh sleep chain, the icon-pulse
-- loop, other sbar.exec calls), so a synchronous io.popen here would
-- hang wait4 forever and wedge the single-threaded event loop. The list
-- is fetched asynchronously via sbar.exec inside refresh_from_pins and
-- the parsed map cached here; rebuild_session_items and the event
-- handlers read this cache. See parse_zmx_list below.
local zmx_attached = {}

-- Track current per-session popup rows so we can wipe them cleanly on
-- rebuild. sketchybar has no "remove by prefix" lua primitive.
local overflow_children = {}

local function clear_overflow_children()
  for _, name in ipairs(overflow_children) do
    sbar.remove(name)
  end
  overflow_children = {}
end

local function add_overflow_child(name, props)
  table.insert(overflow_children, name)
  return sbar.add("item", name, props)
end

local function item_suffix(id)
  -- Use the FULL sanitized id as the bar-item key. Truncating to 8 chars
  -- could collide two session ids sharing an 8-char prefix, silently
  -- overwriting one row (session ids are ULID/UUID-length; item names
  -- have no practical length limit). Parens truncate gsub's
  -- (string, count) multi-return to the string alone so the count can't
  -- leak into a caller's concat/name.
  return (tostring(id or ""):gsub("[^%w_]", "_"))
end

-- Hover-driven popup with a small close grace so a cursor sweep from
-- the parent pill into the popup doesn't slam it shut mid-traversal.
-- Clicking the pizza toggles an in-memory pin that keeps the popup open after
-- the pointer leaves; clicking again unpins it. Track visibility separately so
-- a row refresh cannot hide a popup that was already open.
local OVERFLOW_CLOSE_DELAY_S = 0.6
local overflow_close_token = 0
local overflow_popup_pinned = false
local overflow_popup_visible = false

local function open_overflow_popup()
  overflow_close_token = overflow_close_token + 1
  overflow_popup_visible = true
  parent:set({ popup = { drawing = "on" } })
end

local function schedule_close_overflow_popup()
  overflow_close_token = overflow_close_token + 1
  local my = overflow_close_token
  sbar.exec(string.format("sleep %.2f", OVERFLOW_CLOSE_DELAY_S), function()
    if overflow_close_token == my and not overflow_popup_pinned then
      overflow_popup_visible = false
      parent:set({ popup = { drawing = "off" } })
    end
  end)
end

local function paint_overflow_popup_pin()
  parent:set({
    background = {
      color = overflow_popup_pinned and Colors.pill_bg or Colors.transparent,
      border_color = overflow_popup_pinned and Colors.yellow or Colors.transparent,
    },
  })
end

local function toggle_overflow_popup_pin()
  overflow_popup_pinned = not overflow_popup_pinned
  overflow_close_token = overflow_close_token + 1
  overflow_popup_visible = true
  paint_overflow_popup_pin()
  parent:set({ popup = { drawing = "on" } })
end

-- Hovering the pizza opens the popup; clicking toggles the pin. The in-popup
-- rows also subscribe to mouse.entered/exited in rebuild_session_items so a
-- cursor sweep across rows keeps it open.
parent:subscribe("mouse.entered", open_overflow_popup)
parent:subscribe("mouse.exited", schedule_close_overflow_popup)
parent:subscribe("mouse.clicked", toggle_overflow_popup_pin)

-- Pin file directory; declared early so dedupe_sessions can remove
-- loser files. parse_pins / PIN_JQ_CMD further down reference the same path.
local PIN_DIR = (os.getenv("XDG_STATE_HOME") or (os.getenv("HOME") .. "/.local/state"))
  .. "/sketchybar/sessions"

-- Cache of agent_pid -> alive?. Refreshed asynchronously by the `ps`
-- probe in refresh_from_pins. We must NOT check liveness with a live
-- os.execute: SbarLua's os.execute wrapper flips SIGCHLD to SIG_DFL
-- around system(), and any sbar.exec child that exits during that window
-- becomes an unreapable zombie (observed: zombies climbing every
-- refresh until the process table fills). A cached lookup does no fork.
local pid_liveness = {}

local function pid_alive(pid)
  if not pid or pid == 0 then return true end
  local v = pid_liveness[pid]
  -- Unknown pid (never probed -- e.g. a session just added by an
  -- agent_state_change hook) is assumed alive so it is never pruned
  -- before its first probe.
  if v == nil then return true end
  return v
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

-- Workstream is deliberately omitted: the session name already carries
-- it (e.g. `dotfiles.6c17` on the `dotfiles` workstream), so repeating
-- it as ` · dotfiles` is pure noise. Only the meta that isn't already
-- in the name (claimed issue, todo, workers, gates) is appended.
local function compact_meta(s)
  local parts = {}
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
local function parse_zmx_list(text)
  -- Pure parser for `zmx list` output -> map of session-name -> attached?.
  -- Never io.popen here (SbarLua's SIGCHLD=SIG_IGN makes pclose's wait4
  -- block forever while other SbarLua children are alive); the output is
  -- captured asynchronously via sbar.exec and this parses the string.
  local m = {}
  for line in tostring(text or ""):gmatch("[^\n]+") do
    local name = line:match("name=(%S+)") or line:match("^(%S+)%s+pid=")
    local clients = line:match("clients=(%d+)")
    if name and clients then
      m[name] = tonumber(clients) > 0
    end
  end
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

-- Detect whether a refresh changes anything rendered by the popup. The pin
-- reader runs every 10 seconds for liveness, but rebuilding unchanged rows can
-- briefly invalidate Sketchybar's hover target and make the popup appear not to
-- open. A stable signature lets unchanged refreshes leave the UI untouched.
local RENDER_FIELDS = {
  "workspace", "window_id", "state", "cwd", "zmx_session", "zmx_short",
  "agent", "agent_pid", "tool_name", "workstream", "claimed_issue",
  "claimed_title", "todo_pending", "todo_in_progress", "venom_active",
  "venom_waiting", "venom_failed", "human_gates", "updated_at", "viewed_at",
}
local last_render_signature = nil

local function render_signature()
  local now = os.time()
  local rows = {}
  for _, entry in ipairs(sorted_sessions()) do
    local s = entry.session
    local values = { tostring(entry.id) }
    for _, field in ipairs(RENDER_FIELDS) do
      values[#values + 1] = tostring(s[field] or "")
    end
    values[#values + 1] = effective_state(s, now)
    values[#values + 1] = tostring(zmx_attached[s.zmx_session or ""] == true)
    rows[#rows + 1] = table.concat(values, "\31")
  end
  return table.concat(rows, "\30")
end

-- Highest-urgency state across all sessions. Drives the pizza color in the
-- menu-bar pill.
local function aggregate_state()
  local top_state, top_rank = "idle", -1
  local now = os.time()
  for _, s in pairs(state.sessions) do
    local st = effective_state(s, now)
    local actionable = is_actionable_state(st)
    if st == "needs-attention" then
      actionable = (now - (s.updated_at or 0)) <= NEEDS_ATTENTION_TTL_S
        and (s.viewed_at or 0) < (s.updated_at or 0)
    end
    local ranked_state = actionable and st or "idle"
    local rank = state.urgency[ranked_state] or 0
    if rank > top_rank then
      top_rank, top_state = rank, ranked_state
    end
  end
  return top_state
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
  clear_overflow_children()

  -- Each session labels itself with its zmx session (preferred —
  -- detached sessions wrapped in parens) or, when no zmx is persisted,
  -- the cwd basename. Prefer zmx_short (helper-computed: zmx_session
  -- with $ZMX_SESSION_PREFIX stripped — e.g. "d.claude.ops.fedf" →
  -- "claude.ops.fedf") so the bar isn't cluttered with the namespace
  -- prefix users already know belongs to them.
  --
  -- zmx_attached is the module-level cache refreshed asynchronously in
  -- refresh_from_pins (see parse_zmx_list for why not io.popen here).
  local function row_name(s)
    local display = (s.zmx_short and s.zmx_short ~= "" and s.zmx_short)
      or (s.zmx_session and s.zmx_session ~= "" and s.zmx_session)
    local base
    if display then
      -- Strip the `pizza.` zmx session prefix for display. The helper's
      -- zmx_short is meant to drop $ZMX_SESSION_PREFIX, but that env var
      -- isn't always visible to the agent process, so the full
      -- `pizza.dotfiles.6c17` can leak through. This gsub is idempotent:
      -- an already-stripped short name has no `pizza.` to remove.
      display = display:gsub("^pizza%.", "")
      -- display_zmx wraps in parens for detached. Pass zmx_session to
      -- the attached-map lookup since `zmx l` reports full names.
      base = display_zmx(display, zmx_attached, s.zmx_session)
    else
      base = basename(s.cwd)
    end
    return base .. compact_meta(s)
  end

  -- Every session becomes a vertical popup row under the parent pizza
  -- pill — nothing is drawn inline anymore. With more agents running
  -- (and a camera-notch eating horizontal bar space), a single hover
  -- dropdown scales far better than a row of inline pills. State is
  -- shown by a filled status dot (icon) before the name, colored by
  -- STATE_COLORS; the name text itself stays plain fg (dim if detached)
  -- so the dot — not colored text — carries the signal.
  local now = os.time()

  -- Uniform row width so a row's background (and its hover highlight)
  -- spans the FULL popup width, not just the text. Fonts.popup is
  -- monospaced, so char-count * a slightly-generous advance safely
  -- overestimates the widest row's pixel width: the widest row never
  -- clips, and every shorter row gets trailing fill so all highlights
  -- are the same full-width rectangle. (`·` in meta is 2 UTF-8 bytes so
  -- #s overcounts slightly — that only pads, never clips.)
  local ROW_CHAR_PX   = 9.5  -- advance per char for Fonts.popup (15pt mono)
  local ROW_DOT_ZONE  = 34   -- dot padding_left + glyph + padding_right
  local ROW_RIGHT_PAD = 16
  -- One pass: compute each row's label once (reused when the row is
  -- built below) and track the widest for the shared row width.
  local rows, max_len = {}, 0
  for _, entry in ipairs(sorted_sessions()) do
    local label = row_name(entry.session)
    rows[#rows + 1] = { entry = entry, label = label }
    if #label > max_len then max_len = #label end
  end
  local row_width = math.floor(ROW_DOT_ZONE + max_len * ROW_CHAR_PX + ROW_RIGHT_PAD)

  for _, r in ipairs(rows) do
    local entry, row_label = r.entry, r.label
    local s = entry.session
    local zmx = s.zmx_session or ""
    local detached = zmx ~= "" and not zmx_attached[zmx]
    local short = item_suffix(entry.id)
    local child = "agent_sessions_overflow." .. short

    -- needs-attention is only a live signal when it's fresh AND the
    -- user hasn't already focused that workspace since the hook fire;
    -- once viewed/stale it reads as plain idle in the list.
    local fresh = (now - (s.updated_at or 0)) <= NEEDS_ATTENTION_TTL_S
    local unviewed = (s.viewed_at or 0) < (s.updated_at or 0)
    local st = effective_state(s, now)
    if st == "needs-attention" and not (fresh and unviewed) then
      st = "idle"
    end

    -- Detached sessions dim the whole row (dot + name) since their
    -- terminal is gone; attached rows get a state-colored dot and
    -- full-strength name text.
    local dot_color   = detached and Colors.dim_dark or (STATE_COLORS[st] or Colors.fg)
    local label_color = detached and Colors.dim or Colors.fg

    local row = add_overflow_child(child, {
      position = "popup." .. parent.name,
      width    = row_width,
      icon = {
        string        = "\u{25cf}",
        font          = Fonts.dot,
        color         = dot_color,
        padding_left  = 12,
        padding_right = 8,
      },
      label = {
        string        = row_label,
        font          = Fonts.popup,
        color         = label_color,
        align         = "left",
        padding_left  = 0,
        padding_right = 14,
      },
      -- Background is ALWAYS drawn (transparent at rest) so it — not the
      -- default row height — sets the popup row stride. That makes the
      -- hover highlight exactly fill the row: stride == highlight height,
      -- no top/bottom gap. Hover only swaps the color.
      background = {
        color         = Colors.transparent,
        corner_radius = 4,
        height        = 28,
        drawing       = "on",
      },
      click_script = make_click_script(s, detached),
    })
    -- Hovering a row keeps the popup open and lightens the full-row
    -- background (popup_hover) as an affordance that it's clickable
    -- (click focuses the window / reopens a detached session). Leaving
    -- restores the transparent (still-drawn) background.
    row:subscribe("mouse.entered", function()
      open_overflow_popup()
      row:set({ background = { color = Colors.popup_hover } })
    end)
    row:subscribe("mouse.exited", function()
      schedule_close_overflow_popup()
      row:set({ background = { color = Colors.transparent } })
    end)
  end

  -- No sessions at all — make sure the popup isn't held open by a
  -- stale draw flag.
  local has_any = next(state.sessions) ~= nil
  if not has_any then
    overflow_popup_pinned = false
    overflow_popup_visible = false
    paint_overflow_popup_pin()
    parent:set({ popup = { drawing = "off" } })
  elseif overflow_popup_visible then
    -- Re-assert visibility after replacing popup rows. Sketchybar can drop the
    -- popup while its children are removed and recreated asynchronously.
    parent:set({ popup = { drawing = "on" } })
  end

  last_render_signature = render_signature()
end

local function repaint_parent()
  local top_state = aggregate_state()
  local color = STATE_COLORS[top_state] or Colors.fg
  parent:set({ icon = { color = color } })
end

-- Calm heartbeat for the parent pizza icon while an agent is actively
-- running/tooling. Sessions no longer render inline, so the parent icon
-- is the only live menu-bar signal — breathing its color between BRIGHT
-- and DIM green (via --animate sin) conveys "work in progress" at a
-- glance without a hard blink. We only pulse when the TOP aggregate
-- state is running/tooling, so a higher-urgency state (error,
-- needs-attention) keeps its steady color instead of a green pulse.
local pulse_alive = false

local function running_is_top()
  local top_state = aggregate_state()
  return top_state == "running" or top_state == "tooling"
end

local function animate_parent_icon(hex, frames)
  sbar.exec(string.format(
    "sketchybar --animate sin %d --set %s icon.color=%s",
    frames, parent.name, hex
  ))
end

local function start_pulse_if_needed()
  if pulse_alive then return end
  if not running_is_top() then return end
  pulse_alive = true
  local bright = true
  local frames = math.floor(PULSE_HALF_S * 60)
  local function tick()
    if not running_is_top() then
      pulse_alive = false
      repaint_parent()  -- restore the steady state color
      return
    end
    bright = not bright
    animate_parent_icon(bright and RUNNING_BG_BRIGHT_HEX or RUNNING_BG_DIM_HEX, frames)
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
  -- synthetic workspace-change events here; doing it on every agent update
  -- can burn CPU and can race with the focused-window title query.
end)

-- Mark sessions on the focused workspace as viewed so their
-- needs-attention underline disappears. AeroSpace's
-- exec-on-workspace-change supplies env.FOCUSED_WORKSPACE; ignore any
-- malformed/manual trigger that omits it.
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
-- `overflow_children` table starts empty, so old items would otherwise
-- become orphans (rendered, but not tracked or rebuilt). After cleanup,
-- restore state.sessions from on-disk pin files (written by the helper
-- on every hook fire) so the bar shows the previously-known agents
-- immediately after a sketchybar reload — rather than starting empty
-- and waiting for each session to fire its next hook. PIN_DIR is
-- declared up-top so dedupe_sessions can also see it.

local function parse_pins(tsv)
  -- Pure parser for the pin-file jq @tsv output -> state.sessions. The jq
  -- pipeline runs asynchronously via sbar.exec in refresh_from_pins;
  -- doing it here with io.popen would hang the event loop (see
  -- parse_zmx_list: SbarLua's SIGCHLD=SIG_IGN makes pclose's wait4 block
  -- until every SbarLua child exits, which never happens).
  state.sessions = {}
  for line in tostring(tsv or ""):gmatch("[^\n]+") do
    local fields = {}
    for value in (line .. "\t"):gmatch("([^\t]*)\t") do
      table.insert(fields, value)
    end
    local sid, ws, win, st, cwd, zmx, zsh, agent, ts, pid =
      fields[1], fields[2], fields[3], fields[4], fields[5], fields[6], fields[7], fields[8], fields[9], fields[10]
    if sid and sid ~= "" and ws ~= "" and st ~= "" then
      local agent_pid = tonumber(pid) or 0
      -- Restore every valid record; dead-pid eviction is handled async by
      -- prune_dead_sessions via the pid_liveness cache (the ps probe in
      -- refresh_from_pins) so no os.execute runs here.
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
    end
  end
  dedupe_sessions()
end

local function log_debug(msg)
  local f = io.open("/tmp/sketchybar-agent-status.log", "a")
  if not f then return end
  f:write(os.date("%Y-%m-%d %H:%M:%S"), " ", tostring(msg), "\n")
  f:close()
end

local function remove_known_session_items()
  -- Steady-state pin refresh: remove the rows we actually tracked last
  -- cycle. That set includes rows for sessions that have since VANISHED
  -- (crash/kill with no SessionEnd "clear" event — noticed only by this
  -- 10s pin refresh). The old restored-ids-only sweep removed rows just
  -- for the freshly-restored ids and leaked the vanished ones until the
  -- next --reload. clear_overflow_children removes exactly the tracked
  -- items and resets the table (so rebuild_session_items' own clear below
  -- is a no-op).
  local had_tracked = #overflow_children > 0
  clear_overflow_children()
  -- Post-`--reload` hotload only: our Lua tracking table starts empty but
  -- sketchybar preserved the old bar items across the reload, so the clear
  -- above found nothing. Sweep the restored ids so rebuild can cleanly re-add
  -- them without colliding with a preserved item. Guarded on had_tracked
  -- so steady-state refreshes don't re-remove already-cleared rows (which
  -- would spam `[!] Remove: Item ... not found` every cycle).
  if not had_tracked then
    for id, _ in pairs(state.sessions) do
      pcall(sbar.remove, "agent_sessions_overflow." .. item_suffix(id))
    end
  end
end

-- jq pipeline over the pin files -> one @tsv record per session. Run
-- asynchronously via sbar.exec (NEVER io.popen -- see parse_zmx_list).
local PIN_JQ_CMD = string.format(
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

-- Guard against overlapping async refreshes. Holds the os.time() the
-- current chain started; reset to 0 when it finishes. A chain that dies
-- without resetting (e.g. a callback that never fires) auto-expires
-- after 30s so refreshes can never wedge permanently.
local refresh_started_at = 0

-- Async pin refresh: read pins (jq) then zmx-attached (zmx list), both
-- via sbar.exec so no synchronous pclose/wait4 runs in the event loop,
-- then rebuild the rows. Chained because the row build needs both.
local function refresh_from_pins()
  local now = os.time()
  if refresh_started_at ~= 0 and (now - refresh_started_at) < 30 then
    return
  end
  refresh_started_at = now
  sbar.exec(PIN_JQ_CMD, function(pins_out)
    local ok, err = pcall(parse_pins, type(pins_out) == "string" and pins_out or "")
    if not ok then log_debug("ERROR pins " .. tostring(err)) end
    -- One async probe carries BOTH liveness (ps over every tracked
    -- agent_pid) and zmx attach state (zmx list), split on a sentinel.
    -- Running them via sbar.exec (not os.execute / io.popen) keeps
    -- SIGCHLD=SIG_IGN intact so children auto-reap -- no zombie leak.
    local pids = {}
    for _, s in pairs(state.sessions) do
      local p = tonumber(s.agent_pid) or 0
      if p ~= 0 then pids[#pids + 1] = tostring(p) end
    end
    local ps_cmd = (#pids > 0)
      and ("ps -o pid= -p " .. table.concat(pids, ","))
      or "true"
    local probe = ps_cmd .. " 2>/dev/null; echo '===PROBESEP==='; zmx list 2>/dev/null"
    sbar.exec(probe, function(probe_out)
      local ok2, err2 = pcall(function()
        probe_out = type(probe_out) == "string" and probe_out or ""
        local sep = probe_out:find("===PROBESEP===", 1, true)
        local ps_txt = sep and probe_out:sub(1, sep - 1) or ""
        local zmx_txt = sep and probe_out:sub(sep + 14) or probe_out
        -- Rebuild the liveness cache: every pid we PROBED is alive iff ps
        -- listed it. Un-probed pids stay unknown (assumed alive).
        pid_liveness = {}
        local alive = {}
        for n in ps_txt:gmatch("%d+") do alive[tonumber(n)] = true end
        for _, p in ipairs(pids) do
          local n = tonumber(p)
          pid_liveness[n] = alive[n] == true
        end
        zmx_attached = parse_zmx_list(zmx_txt)
        local restored = 0
        for _, _ in pairs(state.sessions) do restored = restored + 1 end
        recompute_workspace_state()
        local signature = render_signature()
        if signature ~= last_render_signature then
          remove_known_session_items()
          repaint_parent()
          rebuild_session_items()  -- prune_dead_sessions runs here on fresh pid_liveness
          start_pulse_if_needed()
          log_debug("refresh rebuilt=" .. tostring(restored))
        else
          log_debug("refresh unchanged=" .. tostring(restored))
        end
      end)
      if not ok2 then log_debug("ERROR build " .. tostring(err2)) end
      refresh_started_at = 0
    end)
  end)
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

-- Initial paint before the async restore above completes.
repaint_parent()
