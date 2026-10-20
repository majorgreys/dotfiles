-- Occupied-workspace picker, queried only when opened or while already open.
local sbar = require("sketchybar")
local popup = require("popup")
local summary = require("workspace_list")
local title = require("window_title")
require("bar")
local M = {}

function M.attach(parent)
  local rows, current, generation = {}, nil, 0
  local hover
  local status = sbar.add("item", parent.name .. ".status", {
    position = "popup." .. parent.name,
    icon = { drawing = false },
    label = { string = "Loading workspaces…", font = Fonts.regular,
              color = Colors.dim, padding_left = 12, padding_right = 12 },
    background = { height = 28 },
  })

  local function row_style(row)
    return {
      icon = { color = row.hovered and Colors.accent_fg or
                       (row.current and Colors.accent_bg or Colors.fg) },
      label = { color = row.hovered and Colors.accent_fg or Colors.fg },
      background = { color = row.hovered and Colors.accent_bg or
                             (row.current and Colors.popup_hover or Colors.transparent) },
    }
  end

  local function refresh()
    generation = generation + 1
    local request = generation
    sbar.exec("aerospace list-windows --all --format '%{workspace}%{app-name}' --json 2>/dev/null", function(windows, code)
      if request ~= generation then return end
      if (code and code ~= 0) or type(windows) ~= "table" then
        for _, row in ipairs(rows) do
          row.name, row.hovered = nil, false
          row.item:set({ drawing = false })
        end
        status:set({ drawing = true, label = { string = "Workspace list unavailable" } })
        return
      end
      local entries = summary.summarize(windows, current)
      status:set({ drawing = #entries == 0, label = { string = "No occupied workspaces" } })
      for i, entry in ipairs(entries) do
        if not rows[i] then
          local row = {}
          row.item = sbar.add("item", parent.name .. ".row." .. i, {
            position = "popup." .. parent.name,
            width = 560, -- Equal full-row hit targets, including empty right padding.
            icon = { font = Fonts.bold, width = 42, align = "center",
                     padding_left = 8, padding_right = 6 },
            label = { font = Fonts.regular, padding_left = 0, padding_right = 12 },
            background = { drawing = true, height = 28, corner_radius = 3 },
          })
          hover.watch(row.item, {
            on_enter = function()
              if not row.name then return end
              row.hovered = true
              row.item:set(row_style(row))
            end,
            on_exit = function()
              row.hovered = false
              row.item:set(row_style(row))
            end,
          })
          row.item:subscribe("mouse.clicked", function()
            if not row.name then return end
            hover.close()
            sbar.exec(summary.command(row.name))
          end)
          rows[i] = row
        end
        local row = rows[i]
        row.name, row.current = entry.name, entry.current
        local description = entry.count == 0 and "Empty" or
          string.format("%d window%s • %s", entry.count,
            entry.count == 1 and "" or "s", table.concat(entry.apps, ", "))
        local properties = row_style(row)
        properties.drawing = true
        properties.icon.string = entry.name
        properties.label.string = title.truncate_chars(description, 64)
        row.item:set(properties)
      end
      for i = #entries + 1, #rows do
        rows[i].name, rows[i].hovered = nil, false
        rows[i].item:set({ drawing = false })
      end
    end)
  end

  hover = popup.attach(parent, {
    align = "left", on_open = refresh,
    on_close = function()
      generation = generation + 1
      for _, row in ipairs(rows) do
        if row.hovered then
          row.hovered = false
          row.item:set(row_style(row))
        end
      end
    end,
  })
  hover.watch(status)
  return {
    update = function(workspace)
      local changed = current ~= workspace
      current = workspace
      if hover.is_open() and changed then refresh() end
    end,
  }
end

return M
