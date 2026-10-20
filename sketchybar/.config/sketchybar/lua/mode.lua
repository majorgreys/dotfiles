-- mode.lua — visible indicator and keybinding reference for non-main
-- AeroSpace binding modes.
--
-- AeroSpace emits aerospace_mode_change after every mode transition. Querying
-- the current mode also keeps the indicator correct after a Sketchybar reload.

local sbar = require("sketchybar")
require("bar")

local item_name = "aerospace.mode"
local bindings = {
  { key = "ESC",         action = "reload config + bar; return to main" },
  { key = "R",           action = "reset workspace tree" },
  { key = "F",           action = "toggle floating / tiling" },
  { key = "⌫",           action = "close all windows except focused" },
  { key = "⌥⇧ H/J/K/L", action = "join left / down / up / right" },
}

-- SketchyBar clears items before rerunning the config on reload/hotload.
local mode_item = sbar.add("item", item_name, {
  position = "left",
  drawing = false,
  padding_left = 2,
  padding_right = 4,
  icon = { drawing = false },
  label = {
    string = "AEROSPACE",
    color = Colors.accent_fg,
    font = Fonts.bold,
    padding_left = 8,
    padding_right = 8,
  },
  background = {
    drawing = true,
    color = Colors.yellow,
    height = 26,
    corner_radius = 4,
  },
  click_script = "aerospace mode main",
})

local hover = require("popup").attach(mode_item, { align = "left" })

for i, binding in ipairs(bindings) do
  local row = sbar.add("item", item_name .. ".binding." .. i, {
    position = "popup." .. item_name,
    width = 390,
    icon = {
      string = binding.key,
      font = Fonts.bold,
      color = Colors.yellow,
      width = 110,
      align = "left",
      padding_left = 12,
      padding_right = 8,
    },
    label = {
      string = binding.action,
      font = Fonts.regular,
      color = Colors.fg,
      align = "left",
      padding_left = 0,
      padding_right = 12,
    },
    background = {
      drawing = "on",
      color = Colors.transparent,
      height = 28,
      corner_radius = 0,
    },
  })

  hover.watch(row)
end

local generation = 0

local function clean(s)
  return (s or ""):gsub("^%s+", ""):gsub("%s+$", "")
end

local function paint(mode)
  mode = clean(mode)
  local is_main = mode == "" or mode == "main"

  hover.close()
  mode_item:set({ drawing = not is_main })
end

local function refresh()
  generation = generation + 1
  local this_generation = generation

  sbar.exec("aerospace list-modes --current 2>/dev/null", function(mode)
    if this_generation ~= generation then return end
    paint(mode)
  end)
end

mode_item:subscribe({ "aerospace_mode_change", "forced" }, refresh)
refresh()
