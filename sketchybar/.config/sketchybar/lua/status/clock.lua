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

local function repaint()
  clock:set({
    label = { string = os.date("%a %b %d  %H:%M") },
  })
end

clock:subscribe({"routine", "system_woke", "forced"}, repaint)
repaint()
