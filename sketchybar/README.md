# SketchyBar

Minimal AeroSpace status bar for macOS, configured in Lua through [SbarLua](https://github.com/FelixKratz/SbarLua).

- Left: active workspace number and focused window title. Outside AeroSpace's main mode, a mode indicator with a keybinding popup also appears.
- Hover the workspace number to see occupied workspaces, their window counts, and app names. The current workspace is included even when empty. Click a popup row to switch workspaces. Clicking the workspace marker itself still switches back and forth.
- Right: clock, plus warnings that appear only in exception states: battery below 25% while discharging, muted audio output, and an unreachable network path. Normal audio and network states stay hidden.
- Hover the clock to see a Monday-first calendar of the current month. Brackets mark today. The calendar follows the local date automatically.
- Modus Vivendi Tinted palette, 38-pixel bar, PragmataPro Mono Liga text, and FiraCode Nerd Font icons.
- The old Wi-Fi/SSID widget and agent-status module are present but not loaded.

## Install

Requires SketchyBar **v2.24.1 or newer**, AeroSpace, Lua with its `utf8` library, and the SbarLua native module compiled for that Lua version.

```sh
brew install sketchybar lua stow
brew install --cask font-fira-code-nerd-font
```

Install the licensed PragmataPro Mono Liga font separately. Build and install SbarLua using its upstream installation instructions. `make install` places `sketchybar.so` in `~/.local/share/sketchybar_lua/`, and the entry point adds that directory to Lua's native-module search path. Rebuild SbarLua after an incompatible Lua version change. The optional agent popup uses a custom font built by `scripts/build-claude-font.py`; the active bar does not require that variant.

```sh
cd ~/.dotfiles
stow sketchybar
brew services start sketchybar
```

The AeroSpace package supplies the event hooks and reserves 38 pixels above windows on external displays. Its top gap on the built-in display is zero.

## Configuration

`sketchybarrc` batches setup, loads the modules, and enters SbarLua's long-lived event loop (the loop that keeps running and handles events):

- `lua/bar.lua`: bar geometry, palette, fonts, defaults, and custom events.
- `lua/mode.lua`: non-main AeroSpace mode indicator and hover keybinding reference.
- `lua/workspaces.lua` and `lua/window_title.lua`: active workspace and bounded title formatting.
- `lua/workspace_popup.lua` and `lua/workspace_list.lua`: on-demand workspace picker and pure summary/quoting helpers (functions with no side effects).
- `lua/status/clock.lua`, `lua/status/calendar.lua`, and `lua/calendar.lua`: clock and current-month calendar.
- `lua/status/battery.lua`, `lua/status/volume.lua`, `lua/status/network.lua`, and `lua/status/health.lua`: exception-only status and pure parsers.
- `lua/popup.lua` and `lua/tooltip.lua`: shared hover behavior and tooltips.

There is no shell `plugins.d` loader. To enable another module, add it explicitly with `require(...)` in `sketchybarrc`.

The bar keeps its 38-pixel height and solid Modus background, with 14 pixels of outer padding on each side. The workspace marker stays bright with its blue underline. An 8-pixel gap replaces the title's pipe separator, and the title uses the softer foreground. Hostnames and other title content are not stripped.

Titles keep at most 20 words and 100 Unicode code points, including any ellipsis, and truncation never cuts through a UTF-8 byte sequence. The title and calendar use `font.typographical_width` from v2.24.1 to measure glyph advances (the horizontal width of each character), including spaces.

### Popups

Only one managed popup is open at a time. Selectable workspace rows highlight blue with dark text on hover, then restore their normal or current-workspace style on exit. Each workspace option has a full-width hit area. A 250 ms close delay lets the pointer cross the gap between a parent and its popup rows. Hiding a battery, audio, or network warning, or leaving the AeroSpace mode, closes its popup.

The workspace picker queries `aerospace list-windows --all` when opened. It refreshes if the focused workspace changes while open, discards responses received after closing, and shows an unavailable message if the query fails. It does not poll all windows in the background. Workspace names are shell-quoted before a click runs `aerospace workspace`.

### Events and status checks

AeroSpace pushes:

- `aerospace_workspace_change` with `FOCUSED_WORKSPACE=<workspace>`.
- `aerospace_focus_change` when window focus changes.
- `aerospace_mode_change` when the binding mode changes.

The title also polls every second, because Ghostty split focus can change the window title without an AeroSpace focus event. Unchanged workspace and title values do not send item updates. Wake and forced updates refresh the title too.

The clock checks every 10 seconds. Its date is regular-weight secondary text, and its time is bold primary text, with a 12-pixel gap between them. Both fields share one item, so hovering either one opens the calendar. The clock sends only changed date or time fields to SketchyBar, and the calendar rebuilds only when the local date changes.

The audio indicator reads macOS output mute state on `volume_change`, wake, and forced updates, with a 30-second fallback poll. It does not change volume or device settings. Zero volume without the mute flag does not show a warning.

The network indicator checks macOS default-address reachability for IPv4 and IPv6 using `scutil -r`. It runs every 30 seconds and on Wi-Fi-change, wake, and forced events. One reachable address family is enough to keep the warning hidden. This is a local path check, not an internet, DNS, captive-portal, or VPN-health test. Ethernet is not mistaken for disconnected Wi-Fi. No external host is contacted. Failed or unrecognized status queries do not claim an outage or muted output. Overlapping audio and network events are coalesced into one fresh read, so stale results are not painted.

Shell commands use asynchronous `sbar.exec`. Do not use blocking `io.popen` or `os.execute` in event handlers.

## Reload and checks

```sh
sketchybar --reload
cd ~/.dotfiles
lua sketchybar/tests/first_pass.lua
lua sketchybar/tests/second_pass.lua
lua sketchybar/tests/visual_polish.lua
find sketchybar/.config/sketchybar -name '*.lua' -type f -exec luac -p {} \;
luac -p sketchybar/.config/sketchybar/sketchybarrc
```

The regression checks mock SbarLua. They do not contact the running bar, switch workspaces, or change audio or network settings. They cover UTF-8 title bounds and asynchronous focus updates, calendar dates and layout, workspace summaries and quoting, popup close and re-entry, failed and stale queries, and warning visibility.

Hotload (automatic config reload) is enabled. On reload or hotload, SketchyBar clears items before rerunning the config, so startup modules must not try to remove old items. For a multi-file edit, use `sketchybar --hotload off`, finish editing and checking, then run `sketchybar --reload`. The entry point enables hotload again.

The entry point line-buffers stdout before setup. SbarLua forks for asynchronous commands, and line buffering stops the forked processes from repeatedly flushing inherited startup diagnostics. On this machine, Homebrew service logs are in `/opt/homebrew/var/log/sketchybar/`. After a reload, check that new output stays quiet before rotating an oversized old log.

## Hide the macOS menu bar

Optionally set the macOS menu bar to hide automatically in System Settings. The exact setting location depends on the macOS version.
