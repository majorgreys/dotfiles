-- Right-side clock. Polled every 10 seconds; also repaints on system_woke.

local sbar = require("sketchybar")
require("bar")

local clock = sbar.add("item", "clock", {
  position      = "right",
  icon          = { drawing = false },
  update_freq   = 10,
  padding_left  = 8,
  padding_right = 4,
})

local refresh_calendar = require("status.calendar").attach(clock)
local previous
local function repaint()
  local label = os.date("%a %b %d  %H:%M")
  if label ~= previous then
    clock:set({ label = { string = label } })
    previous = label
  end
  refresh_calendar()
end

clock:subscribe({"routine", "system_woke", "forced"}, repaint)
repaint()
