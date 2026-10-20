-- Shared hover lifecycle. Only one managed popup may be open at a time.
-- Delayed close lets the pointer cross the gap between parent and popup rows.
local sbar = require("sketchybar")
require("bar")
local M = {}
local active

function M.attach(parent, opts)
  opts = opts or {}
  local token, opened = 0, false
  local hover = {}
  parent:set({ popup = {
    topmost = true,
    background = {
      color = Colors.popup_bg, corner_radius = 6,
      border_width = 1, border_color = Colors.popup_border,
    },
    horizontal = false, height = opts.height or 28,
    align = opts.align or "right", y_offset = 4,
  } })

  function hover.close()
    token = token + 1
    if not opened then return end
    opened = false
    if active == hover then active = nil end
    parent:set({ popup = { drawing = false } })
    if opts.on_close then opts.on_close() end
  end

  function hover.open()
    token = token + 1
    if opened then return end
    if active then active.close() end
    active, opened = hover, true
    parent:set({ popup = { drawing = true } })
    if opts.on_open then opts.on_open() end
  end

  function hover.is_open() return opened end

  function hover.schedule_close()
    if not opened then return end
    token = token + 1
    local current = token
    -- Keep this async: blocking here would freeze every bar event handler.
    sbar.exec("sleep 0.25", function()
      if token == current then hover.close() end
    end)
  end

  function hover.watch(item, handlers)
    handlers = handlers or {}
    item:subscribe("mouse.entered", function(env)
      hover.open()
      if handlers.on_enter then handlers.on_enter(env) end
    end)
    item:subscribe("mouse.exited", function(env)
      if handlers.on_exit then handlers.on_exit(env) end
      hover.schedule_close()
    end)
  end

  hover.watch(parent)
  return hover
end

return M
