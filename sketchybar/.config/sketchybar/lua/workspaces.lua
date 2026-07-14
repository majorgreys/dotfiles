-- workspaces.lua — compact AeroSpace focus indicator.
--
-- Renders the active workspace marker and focused window title as separate
-- Sketchybar items. Keeping the marker out of the title's text layout avoids
-- mixing a fixed-width icon with a dynamically measured label.
--
-- IMPORTANT: this runs inside SbarLua's long-lived event loop. Do not use
-- io.popen/os.execute here; all shell reads must go through async sbar.exec.

local sbar = require("sketchybar")
require("bar")

-- Hotload preserves existing items across reloads. Remove both the original
-- ten workspace items and older one-item focus indicator before rebuilding.
for i = 1, 10 do
  pcall(sbar.remove, "space." .. i)
end
pcall(sbar.remove, "space.active")

local underline = {
  color         = Colors.accent_bg,
  height        = 2,
  corner_radius = 0,
  y_offset      = -14,
}

local workspace_item = sbar.add("item", "space.active.workspace", {
  position = "left",
  padding_left = 0,
  padding_right = 0,
  icon = {
    string        = "–",
    padding_left  = 0,
    padding_right = 6,
    width         = 20,
    align         = "center",
    color         = Colors.fg,
    font          = Fonts.bold,
  },
  label = { drawing = false },
  background = underline,
  click_script = "aerospace workspace-back-and-forth",
})

local title_item = sbar.add("item", "space.active.title", {
  position = "left",
  padding_left = 0,
  padding_right = 0,
  icon = { drawing = false },
  label = {
    string        = "| No focused window",
    padding_left  = 0,
    padding_right = 6,
    color         = Colors.fg,
    font          = Fonts.regular,
  },
  background = underline,
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

  workspace_item:set({ icon = { string = workspace } })
  title_item:set({ label = { string = "| " .. title } })
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

local function on_workspace_change(env)
  refresh_from_focused_window(env.FOCUSED_WORKSPACE)
end

local function on_focus_change(env)
  if env.AEROSPACE_WORKSPACE and env.AEROSPACE_WORKSPACE ~= "" then
    refresh_from_workspace(env.AEROSPACE_WORKSPACE)
    return
  end

  refresh_from_focused_window(nil)
end

workspace_item:subscribe("aerospace_workspace_change", on_workspace_change)
workspace_item:subscribe("aerospace_focus_change", on_focus_change)

refresh_from_focused_window(nil)
