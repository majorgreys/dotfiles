-- Right-side wifi. Polled every 30 seconds; repaints on wifi_change and
-- system_woke. macOS 14+ redacts the SSID without Location Services
-- granted, so we just show connection state — no SSID label.

local sbar = require("sketchybar")
require("bar")

local wifi = sbar.add("item", "wifi", {
  position      = "right",
  icon          = { font = Fonts.icon, padding_right = 4 },
  label         = { drawing = false },
  update_freq   = 30,
  padding_left  = 8,
  padding_right = 8,
})

local function repaint()
  sbar.exec("ipconfig getsummary en0 2>/dev/null", function(out)
    out = out or ""
    -- Look for "State : <something>" — connected states are BOUND/RENEW/
    -- REBIND/INFORM. Anything else means not connected.
    local state = out:match("State : (%S+)")
    if state == "BOUND" or state == "RENEW"
       or state == "REBIND" or state == "INFORM" then
      wifi:set({ icon = { string = "󰖩", color = Colors.fg } })
    else
      wifi:set({ icon = { string = "󰖪", color = Colors.dim } })
    end
  end)
end

wifi:subscribe({"routine", "wifi_change", "system_woke", "forced"}, repaint)
repaint()
