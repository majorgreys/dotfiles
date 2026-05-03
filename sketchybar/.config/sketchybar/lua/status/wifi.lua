-- Right-side wifi. Polled every 30 seconds; repaints on wifi_change and
-- system_woke. macOS 14+ redacts the SSID without Location Services
-- granted, so we just show connection state — no SSID label.

local sbar = require("sketchybar")
require("bar")
local tooltip = require("tooltip")

local wifi = sbar.add("item", "wifi", {
  position      = "right",
  icon          = { font = Fonts.icon, padding_right = 4 },
  label         = { drawing = false },
  update_freq   = 30,
  padding_left  = 8,
  padding_right = 8,
})

local set_tooltip = tooltip.attach(wifi, { initial = "Wi-Fi: ?" })

-- macOS 14+ gates SSID access behind per-process Location Services
-- authorization, so `ipconfig getsummary`, `networksetup`, `wdutil`,
-- and `system_profiler` all return "<redacted>" from sketchybar's
-- shells. The Shortcuts app already has the Location grant — a
-- one-action shortcut named "Get Wi-Fi SSID" wrapping the
-- "Get Network Details" action returns the unredacted SSID via the
-- `shortcuts run` CLI. If the shortcut is absent or the call fails,
-- we fall back to "(SSID hidden)".
local SSID_SHORTCUT = "Get Wi-Fi SSID"

local function repaint()
  sbar.exec("ipconfig getsummary en0 2>/dev/null", function(out)
    out = out or ""
    local state = out:match("State : (%S+)")
    local connected = state == "BOUND" or state == "RENEW"
                   or state == "REBIND" or state == "INFORM"
    if not connected then
      wifi:set({ icon = { string = "󰖪", color = Colors.dim } })
      set_tooltip("Wi-Fi: disconnected")
      return
    end
    wifi:set({ icon = { string = "󰖩", color = Colors.fg } })
    -- Run the Shortcut and IP lookup in parallel; assemble the tooltip
    -- when both have returned. `shortcuts run` writes the result to
    -- stdout with a trailing newline.
    local ssid_done, ip_done = false, false
    local ssid, ip = nil, ""
    local function maybe_paint()
      if not (ssid_done and ip_done) then return end
      local parts = { ssid or "(SSID hidden)" }
      if ip ~= "" then table.insert(parts, ip) end
      set_tooltip("Wi-Fi: " .. table.concat(parts, " • "))
    end
    sbar.exec(
      string.format("shortcuts run %q 2>/dev/null", SSID_SHORTCUT),
      function(s)
        s = (s or ""):gsub("^%s+", ""):gsub("%s+$", "")
        if s ~= "" then ssid = s end
        ssid_done = true
        maybe_paint()
      end
    )
    sbar.exec("ipconfig getifaddr en0 2>/dev/null", function(addr)
      ip = (addr or ""):gsub("%s+$", "")
      ip_done = true
      maybe_paint()
    end)
  end)
end

wifi:subscribe({"routine", "wifi_change", "system_woke", "forced"}, repaint)
repaint()
