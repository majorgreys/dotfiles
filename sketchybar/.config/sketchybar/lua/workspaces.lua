-- workspaces.lua — compact AeroSpace focus indicator.
--
-- Renders only the active AeroSpace workspace number, followed by the
-- focused window title. Focus changes are pushed by AeroSpace callbacks;
-- the initial render queries AeroSpace asynchronously.
--
-- IMPORTANT: this runs inside SbarLua's long-lived event loop. Do not use
-- io.popen/os.execute here; all shell reads must go through async sbar.exec.

local sbar = require("sketchybar")
require("bar")

-- Hotload preserves existing items across reloads. Remove the former
-- one-item-per-workspace indicators so a config reload leaves only the
-- compact focus pill.
for i = 1, 10 do
  pcall(sbar.remove, "space." .. i)
end

-- Core Text trims trailing ASCII spaces before Sketchybar measures dynamic
-- text. A non-breaking space reserves a final glyph-width, preventing the
-- rightmost title glyph from being clipped by the rounded background.
local TRAILING_PAD = "\194\160"

local item = sbar.add("item", "space.active", {
  position = "left",
  icon = {
    string        = "–",
    padding_left  = 12,
    padding_right = 6,
    width         = 20,
    align         = "center",
    color         = Colors.fg,
    font          = Fonts.bold,
  },
  label = {
    string        = "| No focused window" .. TRAILING_PAD,
    padding_left  = 0,
    padding_right = 12,
    color         = Colors.fg,
    font          = Fonts.regular,
    max_chars     = 80,
  },
  background = {
    color         = Colors.pill_bg,
    border_color  = Colors.pill_border,
    border_width  = 1,
    height        = 26,
    corner_radius = 8,
    padding_left  = 2,
    padding_right = 2,
  },
  width        = "dynamic",
  click_script = "aerospace workspace-back-and-forth",
})

local generation = 0

local function clean(s)
  return (s or ""):gsub("^%s+", ""):gsub("%s+$", "")
end

local function title_for(app, title)
  title = clean(title)
  if title ~= "" then return title end

  app = clean(app)
  if app ~= "" then return app end

  return "No focused window"
end

local function paint(workspace, title)
  workspace = clean(workspace)
  title = clean(title)

  if workspace == "" then workspace = "–" end
  if title == "" then title = "No focused window" end

  item:set({
    icon = { string = workspace },
    label = { string = "| " .. title .. TRAILING_PAD },
  })
end

local function refresh_from_workspace(workspace)
  generation = generation + 1
  paint(workspace, "No focused window")
end

local function refresh_from_focused_window(fallback_workspace)
  generation = generation + 1
  local this_generation = generation

  sbar.exec("aerospace list-windows --focused --format '%{workspace}%{tab}%{app-name}%{tab}%{window-title}' 2>/dev/null", function(out)
    if this_generation ~= generation then return end

    out = clean(out)
    if out == "" then
      if fallback_workspace and fallback_workspace ~= "" then
        paint(fallback_workspace, "No focused window")
      else
        sbar.exec("aerospace list-workspaces --focused 2>/dev/null", function(ws)
          if this_generation ~= generation then return end
          paint(ws, "No focused window")
        end)
      end
      return
    end

    local workspace, app, title = out:match("^([^\t]*)\t([^\t]*)\t?(.*)$")
    paint(workspace or fallback_workspace, title_for(app, title))
  end)
end

item:subscribe("aerospace_workspace_change", function(env)
  refresh_from_focused_window(env.FOCUSED_WORKSPACE)
end)

item:subscribe("aerospace_focus_change", function(env)
  if env.AEROSPACE_WORKSPACE and env.AEROSPACE_WORKSPACE ~= "" then
    refresh_from_workspace(env.AEROSPACE_WORKSPACE)
    return
  end

  refresh_from_focused_window(nil)
end)

refresh_from_focused_window(nil)
