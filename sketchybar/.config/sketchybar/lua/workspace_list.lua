-- Pure workspace summaries and shell quoting; no AeroSpace or SketchyBar IPC.
local M = {}

function M.summarize(windows, current)
  local by_name = {}
  local function workspace(name)
    if type(name) ~= "string" or name == "" then return nil end
    if not by_name[name] then
      by_name[name] = { name = name, count = 0, apps = {}, seen = {} }
    end
    return by_name[name]
  end
  if type(windows) == "table" then
    for _, window in ipairs(windows) do
      if type(window) == "table" then
        local row = workspace(window.workspace)
        if row then
          row.count = row.count + 1
          local app = window["app-name"]
          if type(app) == "string" and app ~= "" and not row.seen[app] then
            row.seen[app] = true
            row.apps[#row.apps + 1] = app
          end
        end
      end
    end
  end
  workspace(current) -- Include the focused workspace even when it is empty.
  local rows = {}
  for _, row in pairs(by_name) do
    table.sort(row.apps)
    row.current = row.name == current
    row.seen = nil
    rows[#rows + 1] = row
  end
  table.sort(rows, function(a, b)
    local na, nb = tonumber(a.name), tonumber(b.name)
    if na and nb and na ~= nb then return na < nb end
    if (na ~= nil) ~= (nb ~= nil) then return na ~= nil end
    return a.name < b.name
  end)
  return rows
end

function M.command(name)
  assert(type(name) == "string" and name ~= "", "workspace name required")
  -- -- prevents option parsing; POSIX single quoting handles arbitrary names.
  return "aerospace workspace -- '" .. name:gsub("'", "'\\''") .. "'"
end

return M
