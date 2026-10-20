-- Pure month layout. Monday-first, four monospace columns per day.
local M = {}

function M.month(year, month, today)
  local first = os.time({ year = year, month = month, day = 1, hour = 12 })
  local date = os.date("*t", first)
  local days = os.date("*t", os.time({
    year = date.year, month = date.month + 1, day = 0, hour = 12,
  })).day
  local offset = (date.wday + 5) % 7
  local weeks = {}
  for week = 0, math.ceil((offset + days) / 7) - 1 do
    local cells = {}
    for column = 0, 6 do
      local day = week * 7 + column - offset + 1
      if day < 1 or day > days then
        cells[#cells + 1] = "    "
      elseif today and today.year == date.year and today.month == date.month
          and today.day == day then
        cells[#cells + 1] = string.format("[%2d]", day)
      else
        cells[#cells + 1] = string.format(" %2d ", day)
      end
    end
    weeks[#weeks + 1] = table.concat(cells)
  end
  return { title = os.date("%B %Y", first),
           weekdays = " Mo  Tu  We  Th  Fr  Sa  Su ", weeks = weeks }
end

return M
