# SketchyBar

Minimal AeroSpace status bar for macOS, configured in Lua through [SbarLua](https://github.com/FelixKratz/SbarLua).

- Left: active workspace number and focused window title. Outside AeroSpace's main mode, a mode indicator with a keybinding popup also appears.
- Right: clock and a low-battery warning while on battery below 25%.
- Modus Vivendi Tinted palette, 38-pixel bar, PragmataPro Mono Liga text and FiraCode Nerd Font icons.
- Wi-Fi, volume and agent-status modules are present but are not loaded by `sketchybarrc`.

## Install

Requires SketchyBar **v2.24.1 or newer**, AeroSpace, Lua with its `utf8` library, and the SbarLua native module compiled for that Lua version.

```sh
brew install sketchybar lua stow
brew install --cask font-fira-code-nerd-font
```

Install the licensed PragmataPro Mono Liga font separately. Build and install SbarLua using its upstream installation instructions. `make install` places `sketchybar.so` in `~/.local/share/sketchybar_lua/`, and the entry point adds that directory to Lua's native-module search path. Rebuild SbarLua if the installed Lua ABI (application binary interface, the compiled-code compatibility contract) changes. The optional agent popup uses a custom font built by `scripts/build-claude-font.py`; the active bar does not require that variant.

```sh
cd ~/.dotfiles
stow sketchybar
brew services start sketchybar
```

The AeroSpace package supplies the event hooks and reserves 38 pixels above windows on external displays. Its top gap on the built-in display is zero.

## Configuration

`sketchybarrc` batches setup, loads the modules below, and enters SbarLua's long-lived event loop:

- `lua/bar.lua`: bar geometry, palette, fonts, defaults and custom events.
- `lua/mode.lua`: non-main AeroSpace mode indicator and hover keybinding reference.
- `lua/workspaces.lua`: active workspace and focused title.
- `lua/window_title.lua`: pure title truncation helpers.
- `lua/status/clock.lua` and `lua/status/battery.lua`: active right-side items.
- `lua/tooltip.lua`: shared hover popup helper.

There is no shell `plugins.d` loader. To enable another module, add it explicitly with `require(...)` in `sketchybarrc`.

Titles keep at most 20 words and 100 Unicode code points, including any ellipsis; the `| ` separator is additional. This limits long URLs and other single-word titles without cutting UTF-8 byte sequences. The title label uses the `font.typographical_width` measurement from v2.24.1 instead of a trailing non-breaking-space workaround.

### Events

AeroSpace pushes these events:

- `aerospace_workspace_change` with `FOCUSED_WORKSPACE=<workspace>`.
- `aerospace_focus_change` when window focus changes.
- `aerospace_mode_change` when the binding mode changes.

The title also polls every second, because Ghostty split focus can change the window title without an AeroSpace focus event. Unchanged displayed workspace and title values do not send item updates. Wake and forced updates refresh the title too.

Shell commands use asynchronous `sbar.exec`. Do not use blocking `io.popen` or `os.execute` in event handlers.

## Reload and checks

```sh
sketchybar --reload
cd ~/.dotfiles
lua sketchybar/tests/first_pass.lua
find sketchybar/.config/sketchybar -name '*.lua' -type f -exec luac -p {} \;
luac -p sketchybar/.config/sketchybar/sketchybarrc
```

The regression checks mock SbarLua. They do not contact the running bar or change AeroSpace focus.

Hotload (automatic config reload) is enabled. On reload or hotload, SketchyBar clears items before rerunning the config, so startup modules must not try to remove old items. For a multi-file edit, use `sketchybar --hotload off`, finish editing and checking, then run `sketchybar --reload`; the entry point enables hotload again.

The entry point line-buffers stdout before setup. SbarLua forks for asynchronous commands, and line buffering stops the forked processes from repeatedly flushing inherited startup diagnostics. On this machine, Homebrew service logs are in `/opt/homebrew/var/log/sketchybar/`. After a reload, check that new output stays quiet before rotating an oversized old log.

## Hide the macOS menu bar

Optionally set the macOS menu bar to hide automatically in System Settings. The exact setting location depends on the macOS version.
