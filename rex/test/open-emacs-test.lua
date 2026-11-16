-- Pure Lua adapter tests: no GUI, network, shell, or live config changes.
local source = assert(arg[1], "pass the open-emacs.lua path")
local tests, passed = {}, 0
local function test(name, fn) tests[#tests + 1] = {name, fn} end
local function equal(actual, expected)
  assert(actual == expected, tostring(actual) .. " ~= " .. tostring(expected))
end
local function contains(value, expected)
  assert(value:find(expected, 1, true), "missing " .. expected .. " in " .. value)
end
local function fails(fn, expected)
  local ok, err = pcall(fn)
  assert(not ok, "expected failure")
  if expected then contains(tostring(err), expected) end
end
local function fixture(overrides)
  local state = {execs = {}, host = "mac", cwd = "/Users/test/project", home = "/Users/test"}
  local options = {
    local_host_id = "mac", remote_hosts = {remote = "pi-remote"},
    emacsclient = "/opt/homebrew/bin/emacsclient", node = "/usr/bin/node",
  }
  for key, value in pairs(overrides or {}) do options[key] = value end
  _G.rex = {
    action = function(action) state.action = action end,
    call = function(name)
      equal(name, "server.status")
      if state.status_error then return nil, "identity refused" end
      return {host_id = state.host}
    end,
    block = {call = function(creator, method, args)
      equal(creator, "com.superlogical.terminal")
      equal(method, "process")
      equal(args.block_id, "block:one")
      return {home = state.home, foreground = {cwd = state.cwd}}
    end},
    exec = function(argv, opts)
      state.execs[#state.execs + 1] = {argv = argv, opts = opts}
      if argv[1] == options.node then
        return state.resolver_result or {ok = true, stdout = "artifact-response"}
      end
      return state.emacs_result or {ok = true, stdout = "nil"}
    end,
    json = {
      -- Model the documented encoder contract for the strings used below.
      encode = function(value) return string.format("%q", value) end,
      decode = function(value)
        equal(value, "artifact-response")
        return state.resolver_directory and {directory = state.resolver_directory} or {}
      end,
    },
  }
  assert(loadfile(source))()(options)
  state.run = function(args)
    return state.action.run({session_id = "session:one", block_id = "block:one"}, args or {})
  end
  state.expression = function()
    local argv = state.execs[#state.execs].argv
    equal(argv[1], options.emacsclient)
    equal(argv[2], "--socket-name=thbemacs")
    equal(argv[3], "--alternate-editor=false")
    equal(argv[4], "--eval")
    return argv[5]
  end
  return state
end
local function with_open(open, fn)
  local original = io.open
  io.open = open
  local ok, err = pcall(fn)
  io.open = original
  if not ok then error(err, 0) end
end

test("register one file/directory action, not a Markdown viewer", function()
  local state = fixture()
  equal(state.action.name, "open_in_emacs")
  equal(state.action.title, "Open in Emacs…")
  equal(state.action.args.path, "string?")
end)

test("ordinary local paths do not require a quoted-filename handler", function()
  local state = fixture()
  local result = state.run({path = "/Users/test/report.md"})
  equal(result.queued, true)
  equal(result.remote, false)
  contains(state.expression(), '"/Users/test/report.md"')
  assert(not state.expression():find("file-name-quote", 1, true))
  contains(state.expression(), "(run-at-time 0 nil #'thb-open ")
end)

test("handler-shaped local paths cannot become TRAMP requests", function()
  local state = fixture()
  fails(function() state.run({path = "/ssh:untrusted:/literal-local-name"}) end, "handler-like prefix")
  equal(#state.execs, 0)
end)

test("relative targets use the terminal cwd, not server cwd", function()
  local state = fixture()
  state.run({path = "report.md"})
  contains(state.expression(), '"/Users/test/project/report.md"')
end)

test("remote originals use the configured SSH alias", function()
  local state = fixture()
  state.host, state.cwd, state.home = "remote", "/home/bits/project", "/home/bits"
  equal(state.run({path = "/home/bits/report.md"}).remote, true)
  contains(state.expression(), '"/ssh:pi-remote:/home/bits/report.md"')
  assert(not state.expression():find("file-name-quote", 1, true))
end)

test("remote directory picker skips local artifact registration", function()
  local state = fixture({artifact_resolver = "/resolver.mjs"})
  state.host, state.cwd, state.home = "remote", "/home/bits/project", "/home/bits"
  with_open(function() error("must not inspect a local registration") end, function()
    state.run()
    contains(state.expression(), '#\'thb-open nil "/ssh:pi-remote:/home/bits/project"')
    equal(#state.execs, 1)
  end)
end)

test("unknown host does not open a matching local path", function()
  local state = fixture()
  state.host = "unknown"
  fails(function() state.run() end, "No SSH alias configured")
  equal(#state.execs, 0)
end)

test("untrusted alias syntax is rejected", function()
  local state = fixture({remote_hosts = {remote = "bad|hop"}})
  state.host = "remote"
  fails(function() state.run() end, "No SSH alias configured")
  equal(#state.execs, 0)
end)

test("tilde uses the source host's home", function()
  local state = fixture()
  state.host, state.cwd, state.home = "remote", "/home/bits/project", "/home/bits"
  state.run({path = "~/report.md"})
  contains(state.expression(), '"/ssh:pi-remote:/home/bits/report.md"')
end)

test("ordinary local shell starts at its working directory", function()
  local state = fixture({artifact_resolver = "/resolver.mjs"})
  with_open(function() return nil, "missing", 2 end, function()
    state.run()
    contains(state.expression(), '#\'thb-open nil "/Users/test/project"')
    equal(#state.execs, 1)
  end)
end)

test("local Pi registration uses the existing validated resolver", function()
  local state = fixture({artifact_resolver = "/resolver.mjs"})
  state.resolver_directory = "/Users/test/conversation/artifacts"
  with_open(function() return {close = function() end} end, function()
    state.run()
    equal(state.execs[1].argv[2], "/resolver.mjs")
    equal(state.execs[1].argv[3], "session:one")
    equal(state.execs[1].argv[4], "block:one")
    contains(state.expression(), '"/Users/test/conversation/artifacts"')
  end)
end)

test("stale Pi context does not silently fall back to cwd", function()
  local state = fixture({artifact_resolver = "/resolver.mjs"})
  state.resolver_result = {ok = false, stderr = "stale registration"}
  with_open(function() return {close = function() end} end, function()
    fails(function() state.run() end, "stale registration")
    equal(#state.execs, 1)
  end)
end)

test("denied registration reads do not fall back", function()
  local state = fixture({artifact_resolver = "/resolver.mjs"})
  with_open(function() return nil, "permission denied", 13 end, function()
    fails(function() state.run() end, "permission denied")
    equal(#state.execs, 0)
  end)
end)

test("control characters in filenames are rejected", function()
  local state = fixture()
  fails(function() state.run({path = "one\ntwo"}) end, "non-empty filename")
  equal(#state.execs, 0)
end)

test("missing terminal context does not inspect another pane", function()
  local state = fixture()
  fails(function() state.action.run({}, {}) end, "Focus a terminal")
  equal(#state.execs, 0)
end)

test("failed host inspection does not open files", function()
  local state = fixture()
  state.status_error = true
  fails(function() state.run() end, "identity refused")
  equal(#state.execs, 0)
end)

test("failed emacsclient request is not reported as queued", function()
  local state = fixture()
  state.emacs_result = {ok = false, stderr = "daemon unavailable"}
  fails(function() state.run({path = "report.md"}) end, "daemon unavailable")
  equal(#state.execs, 1)
end)

test("spaces, quotes and unicode remain one encoded argv value", function()
  local state = fixture()
  state.run({path = 'report " λ.md'})
  local argv = state.execs[1].argv
  equal(#argv, 5)
  contains(argv[5], 'report \\" λ.md')
end)

for _, entry in ipairs(tests) do
  local ok, err = pcall(entry[2])
  if not ok then
    io.stderr:write("FAIL ", entry[1], ": ", tostring(err), "\n")
    os.exit(1)
  end
  passed = passed + 1
  io.write("PASS ", entry[1], "\n")
end
io.write(string.format("%d/%d tests passed\n", passed, #tests))
