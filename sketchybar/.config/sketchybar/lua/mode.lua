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

-- Hotload preserves existing items across reloads.
for i = 1, #bindings do
  pcall(sbar.remove, item_name .. ".binding." .. i)
end
pcall(sbar.remove, item_name)

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
  popup = {
    background = {
      color = Colors.popup_bg,
      corner_radius = 6,
      border_width = 1,
      border_color = Colors.popup_border,
    },
    horizontal = false,
    align = "left",
    y_offset = 4,
  },
  click_script = "aerospace mode main",
})

local CLOSE_DELAY_S = 0.25
local close_token = 0

local function open_popup()
  close_token = close_token + 1
  mode_item:set({ popup = { drawing = "on" } })
end

local function schedule_close_popup()
  close_token = close_token + 1
  local this_token = close_token

  sbar.exec(string.format("sleep %.2f", CLOSE_DELAY_S), function()
    if this_token == close_token then
      mode_item:set({ popup = { drawing = "off" } })
    end
  end)
end

mode_item:subscribe("mouse.entered", open_popup)
mode_item:subscribe("mouse.exited", schedule_close_popup)

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

  row:subscribe("mouse.entered", open_popup)
  row:subscribe("mouse.exited", schedule_close_popup)
end

local generation = 0

local function clean(s)
  return (s or ""):gsub("^%s+", ""):gsub("%s+$", "")
end

local function paint(mode)
  mode = clean(mode)
  local is_main = mode == "" or mode == "main"

  mode_item:set({
    drawing = not is_main,
    popup = { drawing = "off" },
  })
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
