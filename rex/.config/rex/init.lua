-- Open originals in Mac thbemacs; remote hosts use explicit SSH aliases.
require("open-emacs")({
  local_host_id = "87d7a4f6-d26b-4b7e-a5fe-c6fb94ecd823",
  remote_hosts = {
    ["2647dfb9-130a-43cc-8836-4c09288f6f4b"] = "pi-remote",
  },
  emacsclient = "/opt/homebrew/bin/emacsclient",
  node = os.getenv("HOME") .. "/.volta/bin/node",
  artifact_resolver = os.getenv("HOME") .. "/.config/rex/pi-artifacts.mjs",
})

-- Mac-side SSH stays alive while the remote Rex server restarts.
rex.action({
  name = "upgrade_pi_remote_rex",
  title = "Upgrade pi-remote Rex",
  description = "Open an SSH tab; confirm separately before upgrading and restarting.",
  category = "Rex",
  keywords = "pi-remote server update upgrade restart",
  run = function(ctx)
    assert(ctx.session_id, "Select a local Mac Rex session first")
    local script = os.getenv("HOME") .. "/.config/rex/scripts/upgrade-pi-remote.lua"
    local cli = os.getenv("HOME") ..
      "/Library/Application Support/com.superlogical.rex/RexServer/Versions/Current/Rex Server.app/Contents/MacOS/rex"
    return assert(loadfile(script))({
      session = ctx.session_id, mode = "upgrade", rex_cli = cli,
    })
  end,
})

local pi_agent_dir = os.getenv("HOME") .. "/.pi/agent"
dofile(pi_agent_dir .. "/extensions/tb-rex/palette.lua")(pi_agent_dir)

-- Explicit Pi conversation Save/Restore; the controller also works without Pi running.
local rex_recovery_dir = os.getenv("HOME") .. "/dev/rex-agent-recovery"
dofile(rex_recovery_dir .. "/palette.lua")({
  directory = rex_recovery_dir,
  node = os.getenv("HOME") .. "/.volta/bin/node",
  agent_dir = pi_agent_dir,
  suite = pi_agent_dir,
  pi_package = "/opt/homebrew/lib/node_modules/@earendil-works/pi-coding-agent",
})
