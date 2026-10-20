-- Exception-only network warning. This checks macOS path reachability, not
-- internet access or a particular Wi-Fi interface. Wired and IPv6 paths count.
local sbar = require("sketchybar")
local health = require("status.health")
local tooltip = require("tooltip")
require("bar")

local network = sbar.add("item", "network", {
  position = "right", drawing = false, update_freq = 30,
  icon = { string = "󰖪", font = "FiraCode Nerd Font Mono:Regular:16.0", color = Colors.yellow },
  label = { drawing = false }, padding_left = 8, padding_right = 8,
})
local set_tooltip, hover = tooltip.attach(network, { initial = "No reachable network path" })
local last_drawing, in_flight, refresh_pending = false, false, false
local function repaint()
  if in_flight then
    refresh_pending = true
    return
  end
  in_flight = true
  sbar.exec("LC_ALL=C /usr/sbin/scutil -r 0.0.0.0 2>/dev/null && LC_ALL=C /usr/sbin/scutil -r :: 2>/dev/null", function(out, code)
    in_flight = false
    -- A state-change event during the query makes its result stale. Coalesce
    -- the events into one fresh read rather than waiting for the next poll.
    if refresh_pending then
      refresh_pending = false
      repaint()
      return
    end
    local reachable
    if not code or code == 0 then reachable = health.network_reachable(out) end
    local drawing = reachable == false
    if drawing ~= last_drawing then
      network:set({ drawing = drawing })
      last_drawing = drawing
    end
    if not drawing then hover.close() end
    if drawing then set_tooltip("No reachable network path (macOS)") end
  end)
end
network:subscribe({ "routine", "wifi_change", "system_woke", "forced" }, repaint)
repaint()
