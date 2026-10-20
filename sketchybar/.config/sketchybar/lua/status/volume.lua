-- Exception-only audio warning. Normal/unmuted output stays out of the bar.
-- A single async query reads mute state; no volume or device settings change.
local sbar = require("sketchybar")
local health = require("status.health")
local tooltip = require("tooltip")
require("bar")

local volume = sbar.add("item", "volume", {
  position = "right", drawing = false, update_freq = 30,
  icon = { string = "󰝟", font = "FiraCode Nerd Font Mono:Regular:16.0", color = Colors.yellow },
  label = { drawing = false }, padding_left = 8, padding_right = 8,
})
local _, hover = tooltip.attach(volume, { initial = "Audio output is muted" })
local last_drawing, in_flight, refresh_pending = false, false, false
local function repaint()
  if in_flight then
    refresh_pending = true
    return
  end
  in_flight = true
  sbar.exec("osascript -e 'output muted of (get volume settings)' 2>/dev/null", function(out, code)
    in_flight = false
    -- A state-change event during the query makes its result stale. Coalesce
    -- the events into one fresh read rather than waiting for the next poll.
    if refresh_pending then
      refresh_pending = false
      repaint()
      return
    end
    local muted
    if not code or code == 0 then muted = health.muted(out) end
    local drawing = muted == true
    if drawing ~= last_drawing then
      volume:set({ drawing = drawing })
      last_drawing = drawing
    end
    if not drawing then hover.close() end
  end)
end
volume:subscribe({ "volume_change", "routine", "system_woke", "forced" }, repaint)
repaint()
