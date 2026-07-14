-- Right-side low-battery indicator. It appears only while discharging below
-- 25%; charging and adequately charged states stay out of the bar.
-- Polled every 60 seconds and repainted on power_source_change/system_woke.

local sbar = require("sketchybar")
require("bar")
local tooltip = require("tooltip")

local battery = sbar.add("item", "battery", {
  position      = "right",
  icon          = { font = "FiraCode Nerd Font Mono:Regular:16.0" },
  label         = { drawing = false },
  drawing       = false,
  update_freq   = 60,
  padding_left  = 8,
  padding_right = 8,
})

local set_tooltip = tooltip.attach(battery, { initial = "Battery: ?" })

local LOW_BATTERY_PERCENT = 25

local function low_battery_color(percent)
  return percent <= 10 and Colors.red or Colors.yellow
end

-- pmset prints either "<n>:<m> remaining" or "(no estimate)"; we only
-- show the time when it has converged to a real estimate.
local function format_tooltip(percent, charging, out)
  local prefix = charging and "Charging" or "On battery"
  local time = out:match("(%d+:%d%d) remaining")
  if time then
    return string.format("%s: %d%% (%s)", prefix, percent, time)
  end
  return string.format("%s: %d%%", prefix, percent)
end

local function repaint()
  sbar.exec("pmset -g batt", function(out)
    if not out or out == "" then
      battery:set({ drawing = false })
      return
    end
    local percent = tonumber(out:match("(%d+)%%"))
    local charging = out:find("AC Power", 1, true) ~= nil
    if not percent or charging or percent >= LOW_BATTERY_PERCENT then
      battery:set({ drawing = false })
      return
    end

    battery:set({
      drawing = true,
      icon = { string = "󰂃", color = low_battery_color(percent) },
    })
    set_tooltip(format_tooltip(percent, false, out))
  end)
end

battery:subscribe({"routine", "power_source_change", "system_woke", "forced"}, repaint)
repaint()
