-- bar.lua — bar shell, palette, fonts, defaults.
--
-- Loaded once at startup. Exposes the Catppuccin palette via the global
-- `Colors` table and the font set via the global `Fonts` table so other
-- modules can require this file (`require("bar")`) and then read those
-- globals.

local sbar = require("sketchybar")

-- Modus Vivendi Tinted palette — matches the Ghostty / Emacs theme.
-- Reference colors come from the Ghostty theme file at
-- ghostty/.config/ghostty/themes/modus-vivendi-tinted plus the wider
-- Modus Vivendi UI palette (bg-dim/bg-alt etc.) for chrome.
Colors = {
  bg          = 0xff0d0e1c,  -- background (main)
  fg          = 0xffffffff,  -- foreground (main text)
  dim         = 0xff989898,  -- white-faint (less prominent text)
  dim_dark    = 0xff4a4f69,  -- bright black (very dim)
  accent_bg   = 0xff2fafff,  -- blue — focused workspace underline
  accent_fg   = 0xff0d0e1c,
  red         = 0xffff5f59,  -- regular red (urgent error tone)
  yellow      = 0xffd0bc00,  -- regular yellow (caution)
  green       = 0xff44bc44,  -- regular green (active)
  -- Pill / popup chrome — tinted dark elevations of the bg.
  pill_bg     = 0xff1d2235,  -- bg-dim equivalent (slight elevation)
  pill_border = 0xff4a4f69,  -- bright black, subtle outline
  popup_bg    = 0xff13162a,  -- barely-elevated; border carries the edge
  popup_border= 0xff4a4f69,
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
