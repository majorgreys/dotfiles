-- Pure focused-window title formatting shared by the live bar and focused tests.

local M = {}

function M.truncate_words(title, max_words)
  title = title or ""
  max_words = math.max(1, math.floor(tonumber(max_words) or 1))

  local position = 1
  local count = 0
  local cutoff = nil
  while true do
    local first, last = title:find("%S+", position)
    if not first then break end

    count = count + 1
    if count == max_words then cutoff = last end
    if count > max_words then
      return title:sub(1, cutoff):gsub("%s+$", "") .. "…"
    end
    position = last + 1
  end

  return title
end

-- Limit Unicode code points, not bytes. The ellipsis counts toward the limit.
-- Window titles supplied by macOS are UTF-8; this does not split multibyte text.
function M.truncate_chars(title, max_chars)
  title = title or ""
  max_chars = math.max(1, math.floor(tonumber(max_chars) or 1))
  if utf8.len(title) <= max_chars then return title end

  local cutoff = utf8.offset(title, max_chars)
  return title:sub(1, cutoff - 1):gsub("%s+$", "") .. "…"
end

function M.format(title, max_words, max_chars)
  return M.truncate_chars(M.truncate_words(title, max_words), max_chars)
end

return M
