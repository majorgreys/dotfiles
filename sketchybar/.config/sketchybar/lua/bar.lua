-- bar.lua — bar shell, palette, fonts, defaults.
--
-- Loaded once at startup. Exposes the Catppuccin palette via the global
-- `Colors` table and the font set via the global `Fonts` table so other
-- modules can require this file (`require("bar")`) and then read those
-- globals.

local sbar = require("sketchybar")

-- Catppuccin Mocha palette. Hex strings are the same colors used by the
-- prior bash config and the workspace-pill plugin.
Colors = {
  bg          = 0xff000000,
  fg          = 0xffcdd6f4,
  dim         = 0xff7f849c,
  dim_dark    = 0xff6c7086,
  accent_bg   = 0xff89b4fa,
  accent_fg   = 0xff1e1e2e,
  red         = 0xfff38ba8,
  yellow      = 0xfff9e2af,
  green       = 0xffa6e3a1,
  -- Pill / popup chrome.
  pill_bg     = 0xff1e1e2e,  -- catppuccin "base"
  pill_border = 0xff45475a,  -- catppuccin "surface1"
  popup_bg    = 0xff181825,  -- catppuccin "mantle"
  popup_border= 0xff313244,  -- catppuccin "surface0"
  transparent = 0x00000000,
}

Fonts = {
  regular = "PragmataPro Mono Liga:Regular:15.0",
  bold    = "PragmataPro Mono Liga:Bold:15.0",
  dot     = "PragmataPro Mono Liga:Bold:13.0",
  icon    = "FiraCode Nerd Font Mono:Regular:23.0",
  small   = "PragmataPro Mono Liga:Bold:11.0",
}

sbar.bar({
  height           = 38,
  position         = "top",
  sticky           = "on",
  topmost          = "window",
  padding_left     = 6,
  padding_right    = 6,
  color            = Colors.bg,
  border_width     = 0,
  blur_radius      = 0,
  shadow           = "off",
  notch_display_height = 38,
})

sbar.default({
  icon  = { font = Fonts.bold,    color = Colors.fg },
  label = { font = Fonts.regular, color = Colors.fg },
  background = { corner_radius = 4, height = 26 },
  padding_left  = 2,
  padding_right = 2,
})

-- Custom event consumed by the workspace pills and claude_status to
-- repaint themselves when sketchybar-set-state writes new state.
sbar.add("event", "claude_agent_state_change")
sbar.add("event", "aerospace_workspace_change")
