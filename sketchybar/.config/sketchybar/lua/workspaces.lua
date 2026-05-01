-- workspaces.lua — i3-style AeroSpace workspace pills 1..10.
--
-- Each pill renders the workspace number plus an optional small dot
-- showing claude-status' agent state for that workspace (yellow=running,
-- red/yellow=needs-attention, dim=idle, hidden=no agent activity).
--
-- Focus state arrives via env.FOCUSED_WORKSPACE on aerospace_workspace_change
-- (AeroSpace's exec-on-workspace-change passes it). Initial render queries
-- AeroSpace asynchronously.

local sbar = require("sketchybar")
require("bar")
local state = require("state")

local function dot_for(state)
  if state == "running" then
    return "●", Colors.green
  elseif state == "needs-attention" then
    return "●", Colors.yellow
  elseif state == "idle" then
    return "●", Colors.dim_dark
  end
  return nil, Colors.transparent
end

local function paint(pill, i, focused)
  local bg = (focused == tostring(i)) and Colors.accent_bg or Colors.transparent
  local label, color = dot_for(state.workspace_state[tostring(i)])
  pill:set({
    background = { color = bg },
    icon       = { color = Colors.fg },
    label      = {
      string  = label or "",
      color   = color,
      drawing = label ~= nil,
    },
  })
end

local pills = {}
for i = 1, 10 do
  local pill = sbar.add("item", "space." .. i, {
    position = "left",
    icon = {
      string        = tostring(i),
      padding_left  = 0,
      padding_right = 2,
      width         = 20,
      align         = "center",
      color         = Colors.fg,
    },
    label = {
      font          = Fonts.dot,
      padding_left  = 0,
      padding_right = 6,
      drawing       = false,
    },
    background = {
      height        = 2,
      corner_radius = 0,
      y_offset      = -14,
      color         = Colors.transparent,
    },
    width        = "dynamic",
    click_script = "aerospace workspace " .. i,
  })

  pills[i] = pill

  -- Both events trigger a repaint. claude_agent_state_change has no
  -- env.FOCUSED_WORKSPACE so we re-query AeroSpace (cheap).
  pill:subscribe({"aerospace_workspace_change", "claude_agent_state_change"},
    function(env)
      if env.FOCUSED_WORKSPACE and env.FOCUSED_WORKSPACE ~= "" then
        paint(pill, i, env.FOCUSED_WORKSPACE)
      else
        sbar.exec("aerospace list-workspaces --focused", function(out)
          out = (out or ""):gsub("%s+$", "")
          paint(pill, i, out)
        end)
      end
    end)
end

-- Initial paint: ask AeroSpace once and broadcast to all pills.
sbar.exec("aerospace list-workspaces --focused", function(out)
  out = (out or ""):gsub("%s+$", "")
  for i = 1, 10 do paint(pills[i], i, out) end
end)
