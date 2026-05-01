-- Right-side volume. Repaints on volume_change. Glyphs from MDI Nerd Font.

local sbar = require("sketchybar")
require("bar")

local volume = sbar.add("item", "volume", {
  position      = "right",
  icon          = { font = Fonts.icon },
  label         = { drawing = false },
  padding_left  = 8,
  padding_right = 8,
})

local function pick(vol, muted)
  if muted or vol == 0 then return "󰝟", Colors.dim end
  if vol >= 66 then return "󰕾", Colors.fg end
  if vol >= 33 then return "󰖀", Colors.fg end
  return "󰕿", Colors.fg
end

local function paint(vol, muted)
  local icon, color = pick(vol, muted)
  volume:set({ icon = { string = icon, color = color } })
end

local function query_and_repaint(env)
  -- volume_change events carry env.INFO with the new volume level.
  if env and env.INFO and env.INFO ~= "" then
    local vol = tonumber(env.INFO)
    if vol then
      sbar.exec("osascript -e 'output muted of (get volume settings)'", function(muted_out)
        local muted = (muted_out or ""):find("true") ~= nil
        paint(vol, muted)
      end)
      return
    end
  end
  -- Fall back to a full query.
  sbar.exec("osascript -e 'output volume of (get volume settings)'", function(vol_out)
    local vol = tonumber((vol_out or ""):gsub("%s+$", ""))
    if not vol then
      volume:set({ icon = { string = "󰕿", color = Colors.dim } })
      return
    end
    sbar.exec("osascript -e 'output muted of (get volume settings)'", function(muted_out)
      local muted = (muted_out or ""):find("true") ~= nil
      paint(vol, muted)
    end)
  end)
end

volume:subscribe({"volume_change", "forced"}, query_and_repaint)
query_and_repaint({})
