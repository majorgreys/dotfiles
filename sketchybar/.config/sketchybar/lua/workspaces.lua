-- workspaces.lua — i3-style AeroSpace workspace pills 1..10.
--
-- Each pill renders the workspace number; a focused-workspace pill
-- gets an accent background. Agent-state per workspace is shown via
-- the per-session items next to the agent_sessions parent pill, not
-- via dots on workspace numbers.
--
-- Focus state arrives via env.FOCUSED_WORKSPACE on aerospace_workspace_change
-- (AeroSpace's exec-on-workspace-change passes it). Initial render queries
-- AeroSpace asynchronously.

local sbar = require("sketchybar")
require("bar")

local function paint(pill, i, focused)
  local bg = (focused == tostring(i)) and Colors.accent_bg or Colors.transparent
  pill:set({
    background = { color = bg },
    icon       = { color = Colors.fg },
    label      = { drawing = false },
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

  pill:subscribe("aerospace_workspace_change", function(env)
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
