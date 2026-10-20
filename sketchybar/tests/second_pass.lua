-- Deterministic checks; no live SketchyBar IPC, workspace switches or OS changes.
local root = arg[0]:match("^(.*)/tests/[^/]+$") or "."
package.path = root .. "/.config/sketchybar/lua/?.lua;" .. package.path
local checks = 0
local function equal(actual, expected, message)
  checks = checks + 1
  assert(actual == expected, string.format("%s: expected %q, got %q",
    message, tostring(expected), tostring(actual)))
end
local calendar = require("calendar")
local month = calendar.month(2024, 2, { year = 2024, month = 2, day = 29 })
equal(month.title, "February 2024", "month heading")
equal(#month.weeks, 5, "leap February has five calendar rows")
equal(month.weeks[5]:find("[29]", 1, true) ~= nil, true, "leap day highlighted")
equal(month.weeks[1]:sub(1, 12), string.rep(" ", 12), "Monday-first Thursday offset")
equal(calendar.month(2021, 2).weeks[4]:find("28", 1, true) ~= nil, true, "non-leap February ends at 28")
equal(#calendar.month(2021, 2).weeks, 4, "Monday-first 28-day February fits four rows")
equal(#calendar.month(2026, 3).weeks, 6, "six-week month")
equal(calendar.month(2026, 12).title, "December 2026", "December rollover")
equal(calendar.month(2027, 1).title, "January 2027", "January rollover")
for year = 2023, 2028 do
  for m = 1, 12 do
    local rendered = calendar.month(year, m)
    for _, week in ipairs(rendered.weeks) do equal(#week, 28, "fixed-width calendar row") end
  end
end
local summary = require("workspace_list")
local rows = summary.summarize({
  { workspace = "10", ["app-name"] = "Chrome" },
  { workspace = "2", ["app-name"] = "Ghostty" },
  { workspace = "2", ["app-name"] = "Ghostty" },
  { workspace = "2", ["app-name"] = "Finder" },
  { workspace = "named", ["app-name"] = "Emacs" },
  { workspace = "", ["app-name"] = "Ignored" },
}, "1")
equal(#rows, 4, "occupied plus current empty workspace")
equal(rows[1].name, "1", "current empty workspace present")
equal(rows[1].current, true, "current workspace marked")
equal(rows[1].count, 0, "empty workspace count")
equal(rows[2].name, "2", "numeric sorting before 10")
equal(rows[2].count, 3, "window count preserves multiple windows per app")
equal(table.concat(rows[2].apps, ", "), "Finder, Ghostty", "app names deduplicated and sorted")
equal(rows[3].name, "10", "numeric ordering")
equal(rows[4].name, "named", "named workspace retained")
equal(summary.command("-odd"), "aerospace workspace -- '-odd'", "option-like workspace safely quoted")
equal(summary.command("a'b; echo bad"), "aerospace workspace -- 'a'\\''b; echo bad'", "shell metacharacters safely quoted")
local health = require("status.health")
equal(health.network_reachable("Reachable\nNot Reachable\n"), true, "IPv4 route sufficient")
equal(health.network_reachable("Not Reachable\nReachable\n"), true, "IPv6 route sufficient")
equal(health.network_reachable("Not Reachable\nNot Reachable\n"), false, "both address families unreachable")
equal(health.network_reachable("Not Reachable\n"), nil, "incomplete output unknown")
equal(health.network_reachable("No network information\n"), nil, "unknown output not an outage")
equal(health.network_reachable(nil), nil, "missing network output unknown")
equal(health.muted("true\n"), true, "muted parser")
equal(health.muted(" false \n"), false, "unmuted parser")
equal(health.muted("error"), nil, "invalid mute output unknown")

-- Mock SbarLua and retain event handlers, properties and asynchronous requests.
local items, pending, changes = {}, {}, {}
local sbar = {}
local function merge(dest, source)
  for key, value in pairs(source) do
    if type(value) == "table" then
      if type(dest[key]) ~= "table" then dest[key] = {} end
      merge(dest[key], value)
    else dest[key] = value end
  end
end
function sbar.add(kind, name, properties)
  assert(kind == "item" and not items[name], "duplicate/unsupported test item: " .. name)
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
function sbar.exec(command, callback)
  pending[#pending + 1] = { command = command, callback = callback }
end
local function respond(pattern, out, code)
  for i, request in ipairs(pending) do
    if request.command:find(pattern, 1, true) then
      table.remove(pending, i)
      assert(request.callback, "request has no callback")
      request.callback(out, code or 0)
      return
    end
  end
  error("no pending command matching " .. pattern)
end
package.loaded.sketchybar = sbar
package.loaded.bar = true
Colors = { popup_bg = 1, popup_border = 2, dim = 3, fg = 4,
           accent_bg = 5, popup_hover = 6, transparent = 0, yellow = 7, accent_fg = 8 }
Fonts = { regular = "Test:Regular:15.0", bold = "Test:Bold:15.0" }
local popup = require("popup")
local a = sbar.add("item", "test.a", {})
local b = sbar.add("item", "test.b", {})
local opens = 0
local ha = popup.attach(a, { on_open = function() opens = opens + 1 end })
local hb = popup.attach(b)
ha.open()
ha.schedule_close()
ha.open()
respond("sleep", "")
equal(ha.is_open(), true, "re-entry cancels delayed close")
equal(opens, 1, "row re-entry does not refresh popup again")
hb.open()
equal(ha.is_open(), false, "opening another popup closes previous")
equal(hb.is_open(), true, "new popup open")
hb.schedule_close()
respond("sleep", "")
equal(hb.is_open(), false, "close after grace interval")

local picker_parent = sbar.add("item", "picker", {})
local picker = require("workspace_popup").attach(picker_parent)
picker.update("1")
picker_parent.callbacks["mouse.entered"]()
respond("list-windows", {
  { workspace = "2", ["app-name"] = "Ghostty" },
  { workspace = "1", ["app-name"] = "Finder" },
})
equal(items["picker.row.1"].properties.icon.string, "1", "live picker first workspace")
equal(items["picker.row.2"].properties.label.string, "1 window • Ghostty", "live picker app summary")
equal(items["picker.row.2"].properties.width, 560, "full-row hit target width")
items["picker.row.1"].callbacks["mouse.entered"]()
equal(items["picker.row.1"].properties.background.color, Colors.accent_bg, "current row highlights on hover")
equal(items["picker.row.1"].properties.label.color, Colors.accent_fg, "hover text has contrast")
items["picker.row.1"].callbacks["mouse.exited"]()
equal(items["picker.row.1"].properties.background.color, Colors.popup_hover, "current row restores active background")
equal(items["picker.row.1"].properties.icon.color, Colors.accent_bg, "current row restores active marker")
items["picker.row.2"].callbacks["mouse.entered"]()
respond("sleep", "")
equal(picker_parent.properties.popup.drawing, true, "moving between options keeps popup open")
equal(items["picker.row.2"].properties.background.color, Colors.accent_bg, "other row highlights on hover")
items["picker.row.2"].callbacks["mouse.exited"]()
equal(items["picker.row.2"].properties.background.color, Colors.transparent, "other row restores normal background")
equal(items["picker.row.2"].properties.label.color, Colors.fg, "other row restores normal text")
items["picker.row.2"].callbacks["mouse.entered"]()
respond("sleep", "")
items["picker.row.2"].callbacks["mouse.clicked"]()
equal(items["picker.row.2"].properties.background.color, Colors.transparent, "closing resets hover style")
equal(pending[#pending].command, "aerospace workspace -- '2'", "row click uses current name")
equal(picker_parent.properties.popup.drawing, false, "row click closes popup")
table.remove(pending) -- This is a mocked switch, never executed.
-- Close before response: stale result must not add or update rows.
picker_parent.callbacks["mouse.entered"]()
picker_parent.callbacks["mouse.exited"]()
respond("sleep", "")
changes = {}
respond("list-windows", { { workspace = "99", ["app-name"] = "Stale" } })
equal(#changes, 0, "closed popup discards in-flight results")
picker_parent.callbacks["mouse.entered"]()
respond("list-windows", "failure", 1)
equal(items["picker.status"].properties.label.string, "Workspace list unavailable", "query failure shown")
equal(items["picker.row.1"].properties.drawing, false, "old rows hidden on query failure")
local before = #pending
items["picker.row.1"].callbacks["mouse.clicked"]()
equal(#pending, before, "hidden stale row cannot switch workspace")

local real_date = os.date
local fake_today = { year = 2024, month = 2, day = 29 }
os.date = function(fmt, time)
  if fmt == "*t" and time == nil then return fake_today end
  return real_date(fmt, time)
end
local clock = sbar.add("item", "calendar_test", {})
local refresh_calendar = require("status.calendar").attach(clock)
equal(items["calendar_test.calendar.1"].properties.label.string, "February 2024", "live calendar heading")
equal(clock.properties.popup.height, 24, "calendar rows use compact native height")
equal(items["calendar_test.calendar.2"].properties.label["font.typographical_width"], true,
  "calendar spacing uses full glyph advances")
changes = {}
refresh_calendar()
equal(#changes, 0, "same date skips calendar redraw")
fake_today = { year = 2024, month = 3, day = 1 }
refresh_calendar()
equal(items["calendar_test.calendar.1"].properties.label.string, "March 2024", "calendar follows date rollover")
os.date = real_date

require("status.volume")
respond("osascript", "false\n")
equal(items.volume.properties.drawing, false, "normal audio hidden")
items.volume.callbacks.volume_change({ INFO = "0" })
respond("osascript", "true\n")
equal(items.volume.properties.drawing, true, "muted audio shown")
items.volume.callbacks["mouse.entered"]()
items.volume.callbacks.volume_change({ INFO = "50" })
respond("osascript", "false\n")
equal(items.volume.properties.drawing, false, "unmuted audio hidden again")
equal(items.volume.properties.popup.drawing, false, "hidden warning closes tooltip")
items.volume.callbacks.routine({})
items.volume.callbacks.volume_change({})
equal(#pending, 1, "mute checks do not overlap")
respond("osascript", "true\n")
equal(items.volume.properties.drawing, false, "stale mute response does not repaint")
equal(#pending, 1, "event during mute query schedules one fresh read")
respond("osascript", "false\n")
items.volume.callbacks.routine({})
respond("osascript", "true\n", 1)
equal(items.volume.properties.drawing, false, "failed audio query does not claim muted")
require("status.network")
respond("scutil", "Reachable\nNot Reachable\n")
equal(items.network.properties.drawing, false, "healthy route hidden")
items.network.callbacks.routine({})
respond("scutil", "Not Reachable\nNot Reachable\n")
equal(items.network.properties.drawing, true, "unreachable path shown")
items.network.callbacks["mouse.entered"]()
items.network.callbacks.wifi_change({})
respond("scutil", "Not Reachable\nReachable\n")
equal(items.network.properties.drawing, false, "IPv6 recovery hides warning")
equal(items.network.properties.popup.drawing, false, "network recovery closes tooltip")
items.network.callbacks.routine({})
respond("scutil", "Not Reachable\nNot Reachable\n", 1)
equal(items.network.properties.drawing, false, "failed network command does not claim disconnected")
items.network.callbacks.routine({})
items.network.callbacks.wifi_change({})
respond("scutil", "Not Reachable\nNot Reachable\n")
equal(items.network.properties.drawing, false, "stale network response does not repaint")
respond("scutil", "Reachable\nNot Reachable\n")
equal(#pending, 0, "all asynchronous checks consumed")

require("mode")
respond("list-modes", "service\n")
equal(items["aerospace.mode"].properties.drawing, true, "service mode indicator shown")
items["aerospace.mode"].callbacks["mouse.entered"]()
equal(items["aerospace.mode"].properties.popup.drawing, true, "mode popup opens")
items["aerospace.mode"].callbacks.aerospace_mode_change({})
respond("list-modes", "main\n")
equal(items["aerospace.mode"].properties.drawing, false, "main mode indicator hidden")
equal(items["aerospace.mode"].properties.popup.drawing, false, "mode change closes popup")
require("status.battery")
respond("pmset", "Now drawing from 'Battery Power'\n -InternalBattery 25%; discharging; 1:30 remaining\n")
equal(items.battery.properties.drawing, false, "25 percent battery stays hidden")
items.battery.callbacks.routine({})
respond("pmset", "Now drawing from 'Battery Power'\n -InternalBattery 24%; discharging; 1:20 remaining\n")
equal(items.battery.properties.drawing, true, "below 25 percent warning shown")
items.battery.callbacks["mouse.entered"]()
items.battery.callbacks.power_source_change({})
respond("pmset", "Now drawing from 'AC Power'\n -InternalBattery 24%; charging;\n")
equal(items.battery.properties.drawing, false, "charging warning hidden")
equal(items.battery.properties.popup.drawing, false, "charging closes battery tooltip")
equal(#pending, 0, "all status and mode callbacks consumed")
print(string.format("Passed %d checks (calendar, workspace picker, popup lifecycle, audio/network status)", checks))
