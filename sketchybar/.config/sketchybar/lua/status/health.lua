-- Parse local OS status without running commands or changing system settings.
local M = {}

function M.network_reachable(out)
  if type(out) ~= "string" then return nil end
  local unreachable = 0
  -- Two scutil -r results: IPv4 and IPv6 default-address reachability.
  -- One reachable family is sufficient; both must explicitly be unreachable
  -- before showing a warning. Missing/unknown output is not an outage.
  for line in out:gmatch("[^\r\n]+") do
    if line:match("^Reachable") then return true end
    if line:match("^Not Reachable") then unreachable = unreachable + 1 end
  end
  if unreachable == 2 then return false end
  return nil -- Unknown probe output is not proof of an outage.
end

function M.muted(out)
  if type(out) ~= "string" then return nil end
  out = out:match("^%s*(.-)%s*$")
  if out == "true" then return true end
  if out == "false" then return false end
  return nil
end

return M
