-- tooltip.lua — attach a hover-driven tooltip popup to a sketchybar item.
--
-- Sketchybar has no native tooltip primitive, so we reuse its popup
-- machinery: configure a popup on the parent item, add a single label
-- child positioned in that popup, and toggle popup.drawing on/off via
-- mouse.entered / mouse.exited. A short grace period before closing
-- prevents the popup from snapping shut when the cursor crosses from
-- the parent into the popup body. Both the parent and the popup label
-- subscribe to the same handlers so the popup stays open while hovered.

local sbar = require("sketchybar")
require("bar")

local CLOSE_DELAY_S = 0.25
local tokens = {}

local function attach(item, opts)
  opts = opts or {}
  local key = item.name
  tokens[key] = 0

  item:set({
    popup = {
      background = {
        color         = Colors.popup_bg,
        corner_radius = 6,
        border_width  = 1,
        border_color  = Colors.popup_border,
      },
      horizontal = false,
      align      = opts.align or "right",
      y_offset   = 4,
    },
  })

  local label_name = key .. ".tooltip"
  local label_item = sbar.add("item", label_name, {
    position = "popup." .. key,
    icon  = { drawing = false },
    label = {
      string        = opts.initial or "",
      font          = "PragmataPro Mono Liga:Regular:12.0",
      color         = Colors.fg,
      padding_left  = 10,
      padding_right = 10,
    },
    background = {
      height        = 22,
      corner_radius = 0,
      border_width  = 0,
      color         = Colors.transparent,
    },
  })

  local function open()
    tokens[key] = tokens[key] + 1
    item:set({ popup = { drawing = "on" } })
  end

  local function schedule_close()
    tokens[key] = tokens[key] + 1
    local my = tokens[key]
    sbar.exec(string.format("sleep %.2f", CLOSE_DELAY_S), function()
      if tokens[key] == my then
        item:set({ popup = { drawing = "off" } })
      end
    end)
  end

  item:subscribe("mouse.entered", open)
  item:subscribe("mouse.exited", schedule_close)
  label_item:subscribe("mouse.entered", open)
  label_item:subscribe("mouse.exited", schedule_close)

  return function(text)
    label_item:set({ label = { string = text or "" } })
  end
end

return { attach = attach }
