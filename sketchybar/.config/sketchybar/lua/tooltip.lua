-- Shared hover tooltip, with the same close grace period as other popups.
local sbar = require("sketchybar")
local popup = require("popup")
require("bar")

local function attach(item, opts)
  opts = opts or {}
  local previous = opts.initial or ""
  local label = sbar.add("item", item.name .. ".tooltip", {
    position = "popup." .. item.name,
    icon = { drawing = false },
    label = { string = previous, font = "PragmataPro Mono Liga:Regular:12.0",
              color = Colors.fg, padding_left = 10, padding_right = 10 },
    background = { height = 22, corner_radius = 0, border_width = 0,
                   color = Colors.transparent },
  })
  local hover = popup.attach(item, { align = opts.align, height = 22 })
  hover.watch(label)
  return function(text)
    text = text or ""
    if text == previous then return end
    previous = text
    label:set({ label = { string = text } })
  end, hover
end

return { attach = attach }
