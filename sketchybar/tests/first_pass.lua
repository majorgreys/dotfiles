-- Pure Lua regression checks: no SketchyBar/AeroSpace processes or live IPC.
local root = arg[0]:match("^(.*)/tests/[^/]+$") or "."
package.path = root .. "/.config/sketchybar/lua/?.lua;" .. package.path
local title = require("window_title")
local checks = 0
local function equal(actual, expected, message)
  checks = checks + 1
  assert(actual == expected, string.format("%s: expected %q, got %q",
    message, tostring(expected), tostring(actual)))
end

local words = {}
for i = 1, 21 do words[i] = "word" .. i end
local twenty_words = table.concat(words, " ", 1, 20)
equal(title.format("Short title", 20, 100), "Short title", "short title unchanged")
equal(title.format("", 20, 100), "", "empty title")
equal(title.format(nil, 20, 100), "", "nil title")
equal(title.truncate_words(table.concat(words, " "), 20), twenty_words .. "…",
  "existing word-friendly truncation")
equal(title.format(string.rep("x", 500), 20, 100), string.rep("x", 99) .. "…",
  "long single word bounded")
equal(title.format(string.rep("x", 100), 20, 100), string.rep("x", 100),
  "exact character boundary")
equal(title.format(string.rep("界", 101), 20, 100), string.rep("界", 99) .. "…",
  "multibyte title not split")
equal(title.format(string.rep("🚀", 101), 20, 100), string.rep("🚀", 99) .. "…",
  "four-byte code points not split")
equal(title.truncate_chars("abc", 1), "…", "ellipsis included in limit")
equal(title.truncate_chars("ab cde", 4), "ab…", "trim whitespace before ellipsis")
local bounded = title.format(table.concat(words, " "), 20, 40)
equal(utf8.len(bounded) <= 40, true, "combined word and code-point bounds")
equal(bounded:sub(-6) == "……", false, "no duplicate ellipsis")

-- Mock SbarLua's asynchronous API. Loading workspaces registers handlers and
-- queues an initial read; tests choose the order in which responses arrive.
local items, pending, paints = {}, {}, {}
local sbar = {}
function sbar.add(kind, name, properties)
  equal(kind, "item", "workspace creates only items")
  local item = { name = name, properties = properties, callbacks = {} }
  function item:set(properties_)
    paints[#paints + 1] = { name = self.name, properties = properties_ }
  end
  function item:subscribe(events, callback)
    if type(events) == "string" then events = { events } end
    for _, event in ipairs(events) do self.callbacks[event] = callback end
  end
  items[name] = item
  return item
end
function sbar.exec(command, callback)
  pending[#pending + 1] = { command = command, callback = callback }
end
function sbar.remove()
  error("reload must not remove items that SketchyBar already cleared")
end
package.loaded.sketchybar = sbar
package.loaded.bar = true
Colors = { accent_bg = 1, fg = 2 }
Fonts = { bold = "Test:Bold:15.0", regular = "Test:Regular:15.0" }
require("workspaces")
local workspace_item = items["space.active.workspace"]
local title_item = items["space.active.title"]
local function respond(out, index)
  local request = table.remove(pending, index or 1)
  assert(request, "missing asynchronous request")
  request.callback(out)
end
local function routine(out)
  paints = {}
  title_item.callbacks.routine({})
  respond(out)
end

equal(title_item.properties.label["font.typographical_width"], true,
  "typographical-width measurement enabled")
equal(title_item.properties.label.string:find("\194\160", 1, true), nil,
  "no non-breaking-space workaround")
equal(title_item.properties.label.max_chars, 0, "native max_chars stays constant")
equal(title_item.properties.update_freq, 1, "Ghostty polling preserved")
respond("1\tGhostty\tA title\n")
equal(#paints, 2, "first response paints workspace and title")
routine("1\tGhostty\tA title\n")
equal(#paints, 0, "unchanged polling sends no item updates")
routine("2\tGhostty\tA title\n")
equal(#paints, 1, "workspace-only change updates one item")
equal(paints[1].name, workspace_item.name, "workspace update targets marker")
routine("2\tGhostty\tAnother title\n")
equal(#paints, 1, "title-only change updates one item")
equal(paints[1].name, title_item.name, "title update targets label")
equal(paints[1].properties.label.string, "| Another title", "title label content")
routine("2\tGhostty\t" .. string.rep("x", 500) .. "\n")
equal(utf8.len(paints[1].properties.label.string), 102, "100 code points plus separator")
routine("2\tGhostty\t" .. string.rep("x", 501) .. "\n")
equal(#paints, 0, "different raw titles with identical displayed text skip paints")

paints = {}
workspace_item.callbacks.aerospace_focus_change({})
workspace_item.callbacks.aerospace_workspace_change({ FOCUSED_WORKSPACE = "3" })
respond("3\tChrome\tNewest\n", 2)
respond("2\tGhostty\tStale\n")
equal(#paints, 2, "late response cannot repaint stale state")
equal(paints[2].properties.label.string, "| Newest", "newest event wins")

paints = {}
workspace_item.callbacks.aerospace_workspace_change({ FOCUSED_WORKSPACE = "4" })
respond("")
equal(#pending, 0, "empty workspace uses event fallback without extra query")
equal(paints[1].properties.icon.string, "4", "fallback workspace")
equal(paints[2].properties.label.string, "| No focused window", "empty workspace title")
routine("4\tFinder\t\n")
equal(paints[1].properties.label.string, "| Finder", "empty title falls back to app")

equal(type(title_item.callbacks.system_woke), "function", "wake refresh registered")
equal(type(title_item.callbacks.forced), "function", "forced refresh registered")
print(string.format("Passed %d checks (title formatting and asynchronous workspace updates)", checks))
