-- Mac-side palette adapter. Rex routes pane API calls to ctx.server when remote;
-- rex.exec remains on the Mac that owns this configuration.
return function(options)
  assert(type(options) == "table", "Open in Emacs requires configuration")
  assert(type(options.local_host_id) == "string" and options.local_host_id ~= "",
    "Configure the Mac Rex server's host ID")
  assert(type(options.emacsclient) == "string" and options.emacsclient:sub(1, 1) == "/",
    "Configure an absolute emacsclient path")
  if options.artifact_resolver then
    assert(type(options.node) == "string" and options.node:sub(1, 1) == "/"
      and type(options.artifact_resolver) == "string" and options.artifact_resolver:sub(1, 1) == "/",
      "Configure absolute Node and artifact-resolver paths")
  end

  local function clean(value)
    return type(value) == "string" and #value > 0 and #value <= 8192
      and not value:find("[%z\1-\31\127]")
  end
  local function absolute(value)
    return clean(value) and value:sub(1, 1) == "/"
  end
  local function need(value, message)
    if not value then error(message, 0) end
    return value
  end
  local function lisp_string(value)
    return need(rex.json.encode(value), "Cannot encode Emacs filename")
  end
  local function local_artifacts(ctx, home)
    local registration = home .. "/.pi/agent/sessions/.rex-agent-recovery/live/" .. ctx.block_id .. ".json"
    local file, err, code = io.open(registration, "rb")
    if not file then
      -- Only a missing registration is an ordinary terminal. A denied read is
      -- not permission to guess a conversation or silently change context.
      if code == 2 then return nil end
      error(err or "Cannot inspect the Pi registration", 0)
    end
    file:close()
    local result, exec_err = rex.exec({options.node, options.artifact_resolver,
      ctx.session_id, ctx.block_id}, {timeout = 15})
    need(result and result.ok, (result and result.stderr) or exec_err or "Cannot resolve Pi artifacts")
    local target, decode_err = rex.json.decode(result.stdout)
    need(type(target) == "table" and absolute(target.directory),
      decode_err or "Invalid Pi artifacts directory")
    return target.directory
  end

  rex.action{
    name = "open_in_emacs",
    title = "Open in Emacs…",
    description = "Browse files or directories in GUI thbemacs, then focus the selected buffer.",
    category = "Pane",
    keywords = "open file directory dired emacs thbemacs artifacts remote",
    args = {path = "string?"},
    run = function(ctx, args)
      need(clean(ctx.session_id) and ctx.session_id:match("^session:[%w%-]+$")
        and clean(ctx.block_id) and ctx.block_id:match("^block:[%w%-]+$"),
        "Focus a terminal before opening files in Emacs")
      local status, status_err = rex.call("server.status", {})
      need(status and clean(status.host_id), status_err or "Cannot identify the terminal's host")
      local process, process_err = rex.block.call("com.superlogical.terminal", "process", {
        block_id = ctx.block_id,
      })
      need(process, process_err or "Cannot inspect the focused terminal")
      local cwd = process.foreground and process.foreground.cwd
      need(absolute(cwd) and absolute(process.home), "Cannot determine the focused terminal's directory")

      local remote = status.host_id ~= options.local_host_id
      local alias = remote and (options.remote_hosts or {})[status.host_id] or nil
      if remote then
        need(clean(alias) and alias:match("^[%w][%w_.-]*$"),
          "No SSH alias configured for this Rex host; no local path was opened")
      end

      local path = args.path
      if path ~= nil then
        need(clean(path), "The target must be a non-empty filename")
        if path == "~" then
          path = process.home
        elseif path:sub(1, 2) == "~/" then
          path = process.home .. path:sub(2)
        elseif path:sub(1, 1) ~= "/" then
          path = cwd .. "/" .. path
        end
      elseif not remote and options.artifact_resolver then
        cwd = local_artifacts(ctx, process.home) or cwd
      end

      local function filename(value)
        if remote then
          return lisp_string("/ssh:" .. alias .. ":" .. value)
        end
        -- A host-local request must never turn into an Emacs handler/remote
        -- request. Reject handler-shaped root names; ordinary absolute local
        -- names need no /: quoting (which older running daemons may not handle).
        need(not value:match("^/[^/]*:"),
          "Local path has an Emacs handler-like prefix; no remote file was opened")
        return lisp_string(value)
      end
      local expression = "(progn (require 'thb-open) (run-at-time 0 nil #'thb-open "
        .. (path and filename(path) or "nil") .. " " .. filename(cwd) .. ") nil)"
      local result, exec_err = rex.exec({options.emacsclient,
        "--socket-name=thbemacs", "--alternate-editor=false", "--eval", expression}, {timeout = 5})
      need(result and result.ok, (result and result.stderr) or exec_err or "Cannot queue the Emacs open request")
      -- The picker and TRAMP prompts run in Emacs after this action returns.
      -- Queued does not claim that the user selected or opened a file.
      return {queued = true, remote = remote}
    end,
  }
end
