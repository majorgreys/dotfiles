-- Right-side clock. Polled every 10 seconds; also repaints on system_woke.

local sbar = require("sketchybar")
require("bar")

local clock = sbar.add("item", "clock", {
  position      = "right",
  -- Date and time share one item/hit area, so the calendar opens over either.
  icon = { drawing = true, font = Fonts.regular, color = Colors.dim,
           padding_left = 0, padding_right = 12,
           ["font.typographical_width"] = true },
  label = { font = Fonts.bold, color = Colors.fg,
            padding_left = 0, padding_right = 0,
            ["font.typographical_width"] = true },
  update_freq   = 10,
  padding_left  = 8,
  padding_right = 0,
})

local refresh_calendar = require("status.calendar").attach(clock)
local previous_date, previous_time
local function repaint()
  local now = os.time()
  local date = os.date("%a %b %d", now)
  local time = os.date("%H:%M", now)
  local properties = {}
  if date ~= previous_date then
    properties.icon = { string = date }
    previous_date = date
  end
  if time ~= previous_time then
    properties.label = { string = time }
    previous_time = time
  end
  if next(properties) then clock:set(properties) end
  refresh_calendar()
end

clock:subscribe({"routine", "system_woke", "forced"}, repaint)
repaint()
