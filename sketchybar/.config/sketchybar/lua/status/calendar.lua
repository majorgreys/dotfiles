-- Current-month hover calendar; rebuilt only when the local date changes.
local sbar = require("sketchybar")
local calendar = require("calendar")
local popup = require("popup")
require("bar")
local M = {}

function M.attach(clock)
  local rows = {}
  for i = 1, 9 do
    rows[i] = sbar.add("item", clock.name .. ".calendar." .. i, {
      position = "popup." .. clock.name,
      icon = { drawing = false },
      label = { font = i == 1 and Fonts.bold or "PragmataPro Mono Liga:Regular:13.0",
                color = i == 2 and Colors.dim or Colors.fg,
                ["font.typographical_width"] = true,
                padding_left = 12, padding_right = 12 },
      background = { height = 24 },
    })
  end
  local last_date
  local function refresh()
    local now = os.date("*t")
    local key = string.format("%04d-%02d-%02d", now.year, now.month, now.day)
    if key == last_date then return end
    last_date = key
    local month = calendar.month(now.year, now.month, now)
    rows[1]:set({ label = { string = month.title } })
    rows[2]:set({ label = { string = month.weekdays } })
    for i = 1, 6 do
      rows[i + 2]:set({ drawing = month.weeks[i] ~= nil,
                        label = { string = month.weeks[i] or "" } })
    end
    rows[9]:set({ label = { string = "[  ] marks today", color = Colors.dim } })
  end
  local hover = popup.attach(clock, { height = 24, on_open = refresh })
  for _, row in ipairs(rows) do hover.watch(row) end
  refresh()
  return refresh
end

return M
