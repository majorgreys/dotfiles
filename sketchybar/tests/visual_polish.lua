-- Layout and clock hierarchy checks. No live bar IPC or desktop changes.
local root = arg[0]:match("^(.*)/tests/[^/]+$") or "."
package.path = root .. "/.config/sketchybar/lua/?.lua;" .. package.path
local checks = 0
local function equal(actual, expected, message)
  checks = checks + 1
  assert(actual == expected, string.format("%s: expected %q, got %q",
    message, tostring(expected), tostring(actual)))
end
local items, changes, geometry = {}, {}, {}
local sbar = {}
local function merge(dest, source)
  for key, value in pairs(source) do
    if type(value) == "table" then
      if type(dest[key]) ~= "table" then dest[key] = {} end
      merge(dest[key], value)
    else dest[key] = value end
  end
end
function sbar.bar(properties) merge(geometry, properties) end
function sbar.default() end
function sbar.add(kind, name, properties)
  if kind == "event" then return end
  assert(kind == "item" and not items[name])
  local item = { name = name, properties = {}, callbacks = {} }
  merge(item.properties, properties)
  function item:set(props)
    merge(self.properties, props)
    changes[#changes + 1] = { name = self.name, properties = props }
  end
  function item:subscribe(events, callback)
    if type(events) == "string" then events = { events } end
    for _, event in ipairs(events) do self.callbacks[event] = callback end
  end
  items[name] = item
  return item
end
function sbar.exec() error("clock must not spawn a process while updating") end
package.loaded.sketchybar = sbar
require("bar")
equal(geometry.height, 38, "bar height preserved")
equal(geometry.notch_display_height, 38, "notch display height preserved")
equal(geometry.padding_left, 14, "left outer padding")
equal(geometry.padding_right, 14, "right outer padding")
equal(geometry.color, Colors.bg, "Modus background preserved")
equal(geometry.border_width, 0, "no added hairline")
equal(geometry.blur_radius, 0, "no added blur")
equal(geometry.shadow, "off", "no added shadow")

local real_date = os.date
local fake_date, fake_time = "Tue Oct 20", "11:31"
local fake_today = { year = 2026, month = 10, day = 20 }
os.date = function(fmt, time)
  if fmt == "%a %b %d" then return fake_date end
  if fmt == "%H:%M" then return fake_time end
  if fmt == "*t" and time == nil then return fake_today end
  return real_date(fmt, time)
end
require("status.clock")
local clock = items.clock
equal(clock.properties.icon.string, "Tue Oct 20", "date text in secondary field")
equal(clock.properties.icon.drawing, true, "date text visible")
equal(clock.properties.icon.font, Fonts.regular, "date stays regular weight")
equal(clock.properties.icon.color, Colors.dim, "date uses secondary foreground")
equal(clock.properties.icon.padding_right, 12, "date/time gap")
equal(clock.properties.label.string, "11:31", "time text in primary field")
equal(clock.properties.label.font, Fonts.bold, "time has heavier weight")
equal(clock.properties.label.color, Colors.fg, "time stays bright")
equal(clock.properties.padding_right, 0, "clock relies on bar outer padding")
equal(clock.properties.icon["font.typographical_width"], true, "date uses glyph advances")
equal(clock.properties.label["font.typographical_width"], true, "time uses glyph advances")
equal(type(clock.callbacks["mouse.entered"]), "function", "whole clock opens calendar")
equal(clock.properties.popup.height, 24, "calendar geometry preserved")
changes = {}
clock.callbacks.routine({})
equal(#changes, 0, "unchanged clock does not repaint")
fake_time = "11:32"
clock.callbacks.routine({})
equal(#changes, 1, "minute change updates one clock item")
equal(changes[1].properties.icon, nil, "minute change does not resend date")
equal(changes[1].properties.label.string, "11:32", "minute change updates time")
changes = {}
fake_date = "Wed Oct 21"
fake_today = { year = 2026, month = 10, day = 21 }
clock.callbacks.system_woke({})
equal(clock.properties.icon.string, "Wed Oct 21", "date change updates date")
equal(changes[1].properties.label, nil, "date-only change does not resend time")
equal(items["clock.calendar.6"].properties.label.string:find("[21]", 1, true) ~= nil,
  true, "calendar follows date change")
changes = {}
clock.callbacks.forced({})
equal(#changes, 0, "forced refresh preserves unchanged paint suppression")
os.date = real_date
print(string.format("Passed %d checks (bar spacing and clock text hierarchy)", checks))
