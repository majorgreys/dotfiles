-- Right-side battery. Polled every 60 seconds; also repaints on
-- power_source_change and system_woke. Glyphs from MDI Nerd Font.

local sbar = require("sketchybar")
require("bar")

local battery = sbar.add("item", "battery", {
  position      = "right",
  icon          = { font = "FiraCode Nerd Font Mono:Regular:16.0" },
  label         = { drawing = false },
  update_freq   = 60,
  padding_left  = 8,
  padding_right = 8,
})

local function pick(percent, charging)
  if charging then return "󰂄", Colors.green end
  if percent <= 20 then return "󰂃", Colors.red end
  if percent <= 33 then return "󰁻", Colors.yellow end
  if percent <= 66 then return "󰁾", Colors.fg end
  return "󰁹", Colors.fg
end

local function repaint()
  sbar.exec("pmset -g batt", function(out)
    if not out or out == "" then
      battery:set({ icon = { string = "󱉝", color = Colors.dim } })
      return
    end
    local percent = tonumber(out:match("(%d+)%%"))
    local charging = out:find("AC Power", 1, true) ~= nil
    if not percent then
      battery:set({ icon = { string = "󱉝", color = Colors.dim } })
      return
    end
    local icon, color = pick(percent, charging)
    battery:set({ icon = { string = icon, color = color } })
  end)
end

battery:subscribe({"routine", "power_source_change", "system_woke", "forced"}, repaint)
repaint()
